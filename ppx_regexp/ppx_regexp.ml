(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the  LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ppxlib
open Ast_builder.Default

let error = Location.raise_errorf

let warn ~loc msg e =
  let e_msg = estring ~loc msg in
  let name = {txt = "ocaml.ppwarning"; loc} in
  let payload = PStr [{pstr_desc = Pstr_eval (e_msg, []); pstr_loc = loc}] in
  {e with pexp_attributes = attribute ~loc ~name ~payload :: e.pexp_attributes}

module List = struct
  include List

  let rec fold f = function
   | []      -> fun acc -> acc
   | x :: xs -> fun acc -> fold f xs (f x acc)
end

module Regexp = struct
  include Regexp

  let bindings =
    let rec recurse must_match (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      (match e'.Location.txt with
       | Code _ -> fun acc -> acc
       | Seq es -> List.fold (recurse must_match) es
       | Alt es -> List.fold (recurse false) es
       | Opt e -> recurse false e
       | Repeat ({Location.txt = (i, _); _}, e) ->
          recurse (must_match && i > 0) e
       | Nongreedy e -> recurse must_match e
       | Capture _ -> error ~loc "Unnamed capture is not allowed for %%pcre."
       | Capture_as (idr, e) ->
          fun (nG, bs) ->
            recurse must_match e (nG + 1, (idr, Some nG, must_match) :: bs)
       | Call _ -> error ~loc "(&...) is not implemented for %%pcre.")
    in
    (function
     | {Location.txt = Capture_as (idr, e); _} ->
        recurse true e (0, [idr, None, true])
     | e ->
        recurse true e (0, []))

  let to_string =
    let p_alt, p_seq, p_suffix, p_atom = 0, 1, 2, 3 in
    let delimit_if b s = if b then "(?:" ^ s ^ ")" else s in
    let rec recurse p (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      (match e'.Location.txt with
       | Code s ->
          (* Delimiters not needed as Regexp.parse_exn only returns single
           * chars, csets, and escape sequences. *)
          s
       | Seq es ->
          delimit_if (p > p_seq)
            (String.concat "" (List.map (recurse p_seq) es))
       | Alt es ->
          delimit_if (p > p_alt)
            (String.concat "|" (List.map (recurse p_alt) es))
       | Opt e ->
          delimit_if (p > p_suffix) (recurse p_atom e ^ "?")
       | Repeat ({Location.txt = (i, j_opt); _}, e) ->
          let j_str = match j_opt with None -> "" | Some j -> string_of_int j in
          delimit_if (p > p_suffix)
            (Printf.sprintf "%s{%d,%s}" (recurse p_atom e) i j_str)
       | Nongreedy e -> recurse p_suffix e ^ "?"
       | Capture _ -> error ~loc "Unnamed capture is not allowed for %%pcre."
       | Capture_as (_, e) -> "(" ^ recurse p_alt e ^ ")"
       | Call _ -> error ~loc "(&...) is not implemented for %%pcre.")
    in
    (function
     | {Location.txt = Capture_as (_, e); _} ->
        recurse 0 e
     | e ->
        recurse 0 e)
end

let fresh_var =
  let c = ref 0 in
  fun () -> incr c; Printf.sprintf "_ppx_regexp_%d" !c

let rec is_zero p k =
  (match p.[k] with
   | '0' -> is_zero p (k + 1)
   | '1'..'9' -> false
   | _ -> true)

let rec must_match p i =
  let l = String.length p in
  if i = l then true else
  if p.[i] = '?' || p.[i] = '*' then false else
  if p.[i] = '{' then
    let j = String.index_from p (i + 1) '}' in
    not (is_zero p (i + 1)) && must_match p (j + 1)
  else
    true

let extract_bindings ~pos s =
  let r = Regexp.parse_exn ~pos s in
  let nG, bs = Regexp.bindings r in
  let re_str = Regexp.to_string r in
  let loc = Location.none in
  (estring ~loc re_str, bs, nG)

let rec wrap_group_bindings ~loc rhs offG = function
 | [] -> rhs
 | (varG, iG, mustG) :: bs ->
    let eG = match iG with
     | None ->
        [%expr Re.Group.get _g 0]
     | Some iG ->
        [%expr Re.Group.get _g [%e eint ~loc (offG + iG + 1)]]
    in
    let eG =
      if mustG then eG else
      [%expr try Some [%e eG] with Not_found -> None]
    in
    [%expr
      let [%p ppat_var ~loc varG] = [%e eG] in
      [%e wrap_group_bindings ~loc rhs offG bs]]

let transform_cases ~loc cases =
  let aux case =
    if case.pc_guard <> None then
      error ~loc "Guards are not implemented for match%%pcre."
    else
    Ast_pattern.(parse (pstring __')) loc case.pc_lhs
      begin fun {txt = re_src; loc = {loc_start; loc_end; _}} ->
        let re_offset =
          (loc_end.pos_cnum - loc_start.pos_cnum - String.length re_src) / 2
        in
        let pos = {loc_start with pos_cnum = loc_start.pos_cnum + re_offset} in
        let re, bs, nG = extract_bindings ~pos re_src in
        (re, nG, bs, case.pc_rhs)
      end
  in
  let cases, default_rhs =
    (match List.rev (*_map rewrite_case*) cases with
     | {pc_lhs = {ppat_desc = Ppat_any; _}; pc_rhs; pc_guard = None} :: cases ->
        (cases, pc_rhs)
     | {pc_lhs = {ppat_desc = Ppat_var var; _}; pc_rhs; pc_guard = None} ::
       cases ->
        let rhs =
          [%expr let [%p ppat_var ~loc var] = _ppx_regexp_v in [%e pc_rhs]] in
        (cases, rhs)
     | cases ->
        let open Lexing in
        let pos = loc.Location.loc_start in
        let e0 = estring ~loc pos.pos_fname in
        let e1 = eint ~loc pos.pos_lnum in
        let e2 = eint ~loc (pos.pos_cnum - pos.pos_bol) in
        let e = [%expr raise (Match_failure ([%e e0], [%e e1], [%e e2]))] in
        (cases, warn ~loc "A universal case is recommended for %pcre." e))
  in
  let cases = List.rev_map aux cases in
  let res = pexp_array ~loc (List.map (fun (re, _, _, _) -> re) cases) in
  let comp = [%expr
    let a = Array.map (fun s -> Re.mark (Re.Perl.re s)) [%e res] in
    let marks = Array.map fst a in
    let re = Re.compile (Re.alt (Array.to_list (Array.map snd a))) in
    (re, marks)
  ] in
  let var = fresh_var () in
  let re_binding =
    value_binding ~loc ~pat:(ppat_var ~loc {txt = var; loc}) ~expr:comp
  in
  let e_comp = pexp_ident ~loc {txt = Lident var; loc} in

  let rec handle_cases i offG = function
   | [] -> [%expr assert false]
   | (_, nG, bs, rhs) :: cases ->
      [%expr
        if Re.Mark.test _g (snd [%e e_comp]).([%e eint ~loc i]) then
          [%e wrap_group_bindings ~loc rhs offG bs]
        else
          [%e handle_cases (i + 1) (offG + nG) cases]]
  in
  let cases =
    [%expr
      (match Re.exec_opt (fst [%e e_comp]) _ppx_regexp_v with
       | None -> [%e default_rhs]
       | Some _g -> [%e handle_cases 0 0 cases])]
  in
  (cases, re_binding)

let transformation = object
  inherit [value_binding list] Ast_traverse.fold_map as super

  method! expression e_ext acc =
    let e_ext, acc = super#expression e_ext acc in
    (match e_ext.pexp_desc with
     | Pexp_extension
         ({txt = "pcre"; _}, PStr [{pstr_desc = Pstr_eval (e, _); _}]) ->
        let loc = e.pexp_loc in
        (match e.pexp_desc with
         | Pexp_match (e, cases) ->
            let cases, binding = transform_cases ~loc cases in
            ([%expr let _ppx_regexp_v = [%e e] in [%e cases]], binding :: acc)
         | Pexp_function (cases) ->
            let cases, binding = transform_cases ~loc cases in
            ([%expr fun _ppx_regexp_v -> [%e cases]], binding :: acc)
         | _ ->
            error ~loc "[%%pcre] only applies to match an function.")
     | _ -> (e_ext, acc))
end

let impl str =
  let str, rev_bindings = transformation#structure str [] in
  let re_str =
    let loc = Location.none in
    [%str
      module Ppx_regexp__local = struct
        [%%i pstr_value ~loc Nonrecursive rev_bindings]
      end
      open Ppx_regexp__local]
  in
  re_str @ str

let () = Driver.register_transformation ~impl "ppx_regexp"
