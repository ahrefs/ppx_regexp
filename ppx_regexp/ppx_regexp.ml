(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

let rec debug_ast indent (ast : string Regexp_types.t) =
  let spaces = String.make indent ' ' in
  match ast.txt with
  | Code s -> Printf.printf "%sCode(%S)\n" spaces s
  | Seq exprs ->
    Printf.printf "%sSeq[\n" spaces;
    List.iter (debug_ast (indent + 2)) exprs;
    Printf.printf "%s]\n" spaces
  | Alt exprs ->
    Printf.printf "%sAlt[\n" spaces;
    List.iter (debug_ast (indent + 2)) exprs;
    Printf.printf "%s]\n" spaces
  | Opt expr ->
    Printf.printf "%sOpt(\n" spaces;
    debug_ast (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Repeat (_, expr) ->
    Printf.printf "%sRepeat(\n" spaces;
    debug_ast (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Capture expr ->
    Printf.printf "%sCapture(\n" spaces;
    debug_ast (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Capture_as (name, _, expr) ->
    Printf.printf "%sCapture_as(%s,\n" spaces name.txt;
    debug_ast (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Named_subs (name, alias, _, expr) ->
    Printf.printf "%sNamed_subs(%s, %s,\n" spaces name.txt (match alias with Some a -> a.txt | None -> "None");
    debug_ast (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Call longident ->
    Printf.printf "%sCall(%s)\n" spaces (match longident.txt with Longident.Lident s -> s | _ -> "complex")
  | _ -> Printf.printf "%sOther\n" spaces

open Ppxlib
open Ast_builder.Default

let error = Location.raise_errorf

let warn ~loc msg e =
  let e_msg = estring ~loc msg in
  let name = { txt = "ocaml.ppwarning"; loc } in
  let payload = PStr [ { pstr_desc = Pstr_eval (e_msg, []); pstr_loc = loc } ] in
  { e with pexp_attributes = attribute ~loc ~name ~payload :: e.pexp_attributes }

module List = struct
  include List

  let rec fold f = function [] -> fun acc -> acc | x :: xs -> fun acc -> fold f xs (f x acc)
end

module Ctx = struct
  (* name -> parsed value) *)
  type t = (string, label Regexp_types.t) Hashtbl.t

  let empty () = Hashtbl.create 16
  let find name ctx = Hashtbl.find_opt ctx name
end

module Regexp = struct
  open Regexp_types
  include Regexp

  let bindings =
    let rec recurse must_match (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      match e'.Location.txt with
      | Code _ -> fun acc -> acc
      | Seq es -> List.fold (recurse must_match) es
      | Alt es -> List.fold (recurse false) es
      | Opt e -> recurse false e
      | Repeat ({ Location.txt = i, _; _ }, e) -> recurse (must_match && i > 0) e
      | Nongreedy e -> recurse must_match e
      | Capture _ -> error ~loc "Unnamed capture is not allowed for %%pcre and %%mik."
      | Capture_as (idr, conv, e) ->
        fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Named_subs (idr, None, conv, e) | Named_subs (_, Some idr, conv, e) ->
        fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Unnamed_subs (_, e) -> recurse must_match e
      | Call _ -> error ~loc "(&...) is not implemented for %%pcre and %%mik."
    in
    function
    | { Location.txt = Capture_as (idr, _, e); _ } -> recurse true e (0, [ idr, None, None, true ])
    | e -> recurse true e (0, [])

  let to_string ~ctx =
    let p_alt, p_seq, p_suffix, p_atom = 0, 1, 2, 3 in
    let delimit_if b s = if b then "(?:" ^ s ^ ")" else s in
    let rec recurse p (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      let get_parsed idr =
        let var_name = idr.txt in
        let content =
          match Ctx.find var_name ctx with
          | Some value -> value
          | None ->
            error ~loc "Variable '%s' not found. %%pcre and %%mik only support global let bindings for substitution."
              var_name
        in
        content
      in
      match e'.Location.txt with
      | Code s ->
        (* Delimiters not needed as Regexp.parse_exn only returns single
         * chars, csets, and escape sequences. *)
        s
      | Seq es -> delimit_if (p > p_seq) (String.concat "" (List.map (recurse p_seq) es))
      | Alt es -> delimit_if (p > p_alt) (String.concat "|" (List.map (recurse p_alt) es))
      | Opt e ->
        let content = recurse p_atom e in
        let result = if p >= p_seq then "(?:" ^ content ^ ")?" else content ^ "?" in
        delimit_if (p > p_suffix) result
      | Repeat ({ Location.txt = i, j_opt; _ }, e) ->
        let j_str = match j_opt with None -> "" | Some j -> string_of_int j in
        delimit_if (p > p_suffix) (Printf.sprintf "%s{%d,%s}" (recurse p_atom e) i j_str)
      | Nongreedy e -> recurse p_suffix e ^ "?"
      | Capture _ -> error ~loc "Unnamed capture is not allowed for %%pcre and %%mik."
      | Capture_as (_, _, e) -> "(" ^ recurse p_alt e ^ ")"
      | Named_subs (idr, _, _, _) ->
        let content = get_parsed idr in
        "(" ^ recurse p_alt content ^ ")"
      | Unnamed_subs (idr, _) ->
        let content = get_parsed idr in
        recurse p_alt content
      | Call _ -> error ~loc "(&...) is not implemented for %%pcre and %%mik."
    in
    function { Location.txt = Capture_as (_, _, e); _ } -> recurse 0 e | e -> recurse 0 e
end

let fresh_var =
  let c = ref 0 in
  fun () ->
    incr c;
    Printf.sprintf "_ppx_regexp_%d" !c

let rec is_zero p k = match p.[k] with '0' -> is_zero p (k + 1) | '1' .. '9' -> false | _ -> true

let rec must_match p i =
  let l = String.length p in
  if i = l then true
  else if p.[i] = '?' || p.[i] = '*' then false
  else if p.[i] = '{' then (
    let j = String.index_from p (i + 1) '}' in
    (not (is_zero p (i + 1))) && must_match p (j + 1))
  else true

let extract_bindings ~(parser : ?pos:position -> string -> string Regexp_types.t) ~ctx ~pos s =
  let r = parser ~pos s in
  let nG, bs = Regexp.bindings r in
  let re_str = Regexp.to_string ~ctx r in
  let loc = Location.none in
  estring ~loc re_str, bs, nG

let rec wrap_group_bindings ~loc rhs offG = function
  | [] -> rhs
  | (varG, iG, conv, mustG) :: bs ->
    let eG =
      match iG with
      | None -> [%expr Re.Group.get _g 0]
      | Some iG -> [%expr Re.Group.get _g [%e eint ~loc (offG + iG + 1)]]
    in
    let eG =
      match conv with
      | None -> eG
      | Some Regexp_types.Int -> [%expr int_of_string [%e eG]]
      | Some Regexp_types.Float -> [%expr float_of_string [%e eG]]
      | Some (Regexp_types.Func func_name) ->
        let longident =
          match String.split_on_char '.' func_name with
          | [] -> assert false
          | [ name ] -> Lident name
          | module_path ->
            let rec build_longident = function
              | [] -> assert false
              | [ name ] -> Lident name
              | module_name :: rest -> Ldot (build_longident rest, module_name)
            in
            build_longident (List.rev module_path)
        in
        let func_ident = pexp_ident ~loc { txt = longident; loc } in
        [%expr [%e func_ident] [%e eG]]
    in
    let eG = if mustG then eG else [%expr try Some [%e eG] with Not_found -> None] in
    [%expr
      let [%p ppat_var ~loc varG] = [%e eG] in
      [%e wrap_group_bindings ~loc rhs offG bs]]

let guards_equal g1 g2 =
  match g1, g2 with
  | None, None -> true
  | Some e1, Some e2 ->
    (* TODO: simplified, should use a more sophisticated AST comparison *)
    Pprintast.string_of_expression e1 = Pprintast.string_of_expression e2
  | _ -> false

let group_by_guard cases =
  List.fold_left
    begin
      fun groups case ->
        let guard = case.pc_guard in
        let rec add_to_groups = function
          | [] -> [ guard, [ case ] ]
          | (g, cases) :: rest ->
            if guards_equal g guard then (g, case :: cases) :: rest else (g, cases) :: add_to_groups rest
        in
        add_to_groups groups
    end
    [] cases
  |> List.map (fun (g, cases) -> g, List.rev cases)

let transform_cases ~mode ~opts ~loc ~ctx cases =
  let aux case =
    Ast_pattern.(parse (pstring __'))
      loc case.pc_lhs
      begin
        fun { txt = re_src; loc = { loc_start; loc_end; _ } } ->
          let re_offset = (loc_end.pos_cnum - loc_start.pos_cnum - String.length re_src) / 2 in
          let pos = { loc_start with pos_cnum = loc_start.pos_cnum + re_offset; pos_lnum = loc_end.pos_lnum } in
          let parser =
            match mode with `Pcre -> Regexp.parse_exn ~target:`Match | `Mik -> Regexp.parse_mik_exn ~target:`Match
          in
          let re, bs, nG = extract_bindings ~parser ~pos ~ctx re_src in
          re, nG, bs, case.pc_rhs, case.pc_guard
      end
  in

  let rec separate_defaults acc = function
    | [] -> List.rev acc, []
    | ({ pc_lhs = { ppat_desc = Ppat_any; _ }; _ } as case) :: rest -> List.rev acc, case :: rest
    | ({ pc_lhs = { ppat_desc = Ppat_var _; _ }; _ } as case) :: rest -> List.rev acc, case :: rest
    | case :: rest -> separate_defaults (case :: acc) rest
  in

  let cases, default_cases = separate_defaults [] cases in

  let default_rhs =
    match default_cases with
    | [] ->
      let open Lexing in
      let pos = loc.Location.loc_start in
      let e0 = estring ~loc pos.pos_fname in
      let e1 = eint ~loc pos.pos_lnum in
      let e2 = eint ~loc (pos.pos_cnum - pos.pos_bol) in
      let e = [%expr raise (Match_failure ([%e e0], [%e e1], [%e e2]))] in
      warn ~loc "A universal case is recommended for %pcre and %mik." e
    | _ ->
      let transformed_default_cases =
        List.map
          (fun case ->
            match case.pc_lhs.ppat_desc with
            | Ppat_var var ->
              let new_rhs =
                [%expr
                  let [%p ppat_var ~loc var] = _ppx_regexp_v in
                  [%e case.pc_rhs]]
              in
              { case with pc_lhs = ppat_any ~loc; pc_rhs = new_rhs }
            | _ -> case (* keep _ patterns as-is *))
          default_cases
      in
      (* build a regular match expression for default cases *)
      let default_match = pexp_match ~loc [%expr _ppx_regexp_v] transformed_default_cases in
      default_match
  in

  let grouped_cases = group_by_guard cases in

  let compiled_groups =
    List.map
      begin
        fun (guard, group_cases) ->
          let processed_cases = List.rev_map aux group_cases in
          let res = pexp_array ~loc (List.map (fun (re, _, _, _, _) -> re) processed_cases) in

          let opts_expr =
            let rec opts_to_expr = function
              | [] -> [%expr []]
              | `Caseless :: rest -> [%expr `Caseless :: [%e opts_to_expr rest]]
              | _ -> assert false
            in
            opts_to_expr opts
          in
          let comp =
            [%expr
              let a = Array.map (fun s -> Re.mark (Re.Perl.re ~opts:[%e opts_expr] s)) [%e res] in
              let marks = Array.map fst a in
              let re = Re.compile (Re.alt (Array.to_list (Array.map snd a))) in
              re, marks]
          in
          let var = fresh_var () in
          let re_binding = value_binding ~loc ~pat:(ppat_var ~loc { txt = var; loc }) ~expr:comp in
          let e_comp = pexp_ident ~loc { txt = Lident var; loc } in

          let rec handle_cases i offG = function
            | [] -> [%expr assert false]
            | (_, nG, bs, rhs, _) :: cases ->
              [%expr
                if Re.Mark.test _g (snd [%e e_comp]).([%e eint ~loc i]) then [%e wrap_group_bindings ~loc rhs offG bs]
                else [%e handle_cases (i + 1) (offG + nG) cases]]
          in

          let match_expr =
            [%expr
              match Re.exec_opt (fst [%e e_comp]) _ppx_regexp_v with
              | None -> None
              | Some _g -> Some [%e handle_cases 0 0 processed_cases]]
          in

          guard, match_expr, re_binding
      end
      grouped_cases
  in

  let rec try_groups = function
    | [] -> default_rhs
    | (None, match_expr, _) :: rest ->
      [%expr match [%e match_expr] with Some result -> result | None -> [%e try_groups rest]]
    | (Some guard_expr, match_expr, _) :: rest ->
      [%expr
        if [%e guard_expr] then (match [%e match_expr] with Some result -> result | None -> [%e try_groups rest])
        else [%e try_groups rest]]
  in

  let final_expr = try_groups compiled_groups in
  let all_bindings = List.map (fun (_, _, b) -> b) compiled_groups in

  ( [%expr
      let _ppx_regexp_v = [%e pexp_ident ~loc { txt = Lident "_ppx_regexp_v"; loc }] in
      [%e final_expr]],
    all_bindings )

let check_unbounded_recursion ~mode var_name content =
  let contains_regex pattern str =
    let re = Re.Str.regexp pattern in
    try
      Re.Str.search_forward re str 0 |> ignore;
      true
    with Not_found -> false
  in
  match mode with
  | `Pcre ->
    let u = Printf.sprintf {|\(\?U<%s>\)|} var_name in
    let n = Printf.sprintf {|\(\?N<%s>\)|} var_name in
    let n_as = Printf.sprintf {|\(\?N<%s as [^>]*>\)|} var_name in
    contains_regex u content || contains_regex n content || contains_regex n_as content
  | `Mik ->
    let u = Printf.sprintf {|\(\b%s\b\)|} var_name in
    let n_as = Printf.sprintf {|\(\b%s\b as [^)]*\)|} var_name in
    let n_as_conv = Printf.sprintf {|\(\b%s\b as [^)]*:[^)]*\)|} var_name in
    contains_regex u content || contains_regex n_as content || contains_regex n_as_conv content

let transform_let ~mode ~ctx =
  let parser = match mode with `Pcre -> Regexp.parse_exn ~target:`Let | `Mik -> Regexp.parse_mik_exn ~target:`Let in
  List.map
    begin
      fun vb ->
        match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
        | Ppat_var { txt = var_name; loc }, Pexp_constant (Pconst_string (value, _, _)) ->
          if check_unbounded_recursion ~mode var_name value then error ~loc "Unbounded recursion detected!"
          else begin
            let string_loc = vb.pvb_expr.pexp_loc in
            let pos = string_loc.loc_start in
            let pos = { pos with pos_cnum = pos.pos_cnum + 2 (* skip opening *) } in
            let parsed = parser ~pos value in
            Hashtbl.replace ctx var_name parsed;
            let warning_attr =
              attribute ~loc ~name:{ txt = "ocaml.warning"; loc }
                ~payload:(PStr [ { pstr_desc = Pstr_eval (estring ~loc "-32", []); pstr_loc = loc } ])
            in
            { vb with pvb_attributes = warning_attr :: vb.pvb_attributes }
          end
        | _ -> vb
    end

let transformation ctx =
  object
    inherit [value_binding list] Ast_traverse.fold_map as super

    method! structure_item item acc =
      match item.pstr_desc with
      | Pstr_extension (({ txt = "pcre"; _ }, PStr [ { pstr_desc = Pstr_value (rec_flag, vbs); _ } ]), _) ->
        let bindings = transform_let ~mode:`Pcre ~ctx vbs in
        let new_item = { item with pstr_desc = Pstr_value (rec_flag, bindings) } in
        new_item, acc
      | Pstr_extension (({ txt = "mik"; _ }, PStr [ { pstr_desc = Pstr_value (rec_flag, vbs); _ } ]), _) ->
        let bindings = transform_let ~mode:`Mik ~ctx vbs in
        let new_item = { item with pstr_desc = Pstr_value (rec_flag, bindings) } in
        new_item, acc
      | _ -> super#structure_item item acc

    method! expression e_ext acc =
      let e_ext, acc = super#expression e_ext acc in
      let make_transformations ~mode ~opts ~loc = function
        | Pexp_match (e, cases) ->
          let cases, bindings = transform_cases ~mode ~opts ~loc ~ctx cases in
          ( [%expr
              let _ppx_regexp_v = [%e e] in
              [%e cases]],
            bindings @ acc )
        | Pexp_function cases ->
          let cases, bindings = transform_cases ~mode ~opts ~loc ~ctx cases in
          [%expr fun _ppx_regexp_v -> [%e cases]], bindings @ acc
        | _ -> error ~loc "[%%pcre] and [%%mik] only apply to match, function and global let declarations of strings."
      in
      match e_ext.pexp_desc with
      | Pexp_extension ({ txt = "pcre"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
        let loc = e.pexp_loc in
        make_transformations ~mode:`Pcre ~opts:[] ~loc e.pexp_desc
      | Pexp_extension ({ txt = "pcre_i"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
        let loc = e.pexp_loc in
        make_transformations ~mode:`Pcre ~opts:[ `Caseless ] ~loc e.pexp_desc
      | Pexp_extension ({ txt = "mik"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
        let loc = e.pexp_loc in
        make_transformations ~mode:`Mik ~opts:[] ~loc e.pexp_desc
      | Pexp_extension ({ txt = "mik_i"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
        let loc = e.pexp_loc in
        make_transformations ~mode:`Mik ~opts:[ `Caseless ] ~loc e.pexp_desc
      | _ -> e_ext, acc
  end

let impl str =
  let ctx = Ctx.empty () in
  let str, rev_bindings = (transformation ctx)#structure str [] in
  if rev_bindings = [] then str
  else (
    let re_str =
      let loc = Location.none in
      [%str
        open struct
          [%%i pstr_value ~loc Nonrecursive rev_bindings]
        end]
    in
    re_str @ str)

let () = Driver.register_transformation ~impl "ppx_regexp"
