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

open Ppxlib
open Ast_builder.Default

let make_alias_binding ~loc ~var_name =
  let warning_attr =
    attribute ~loc ~name:{ txt = "warning"; loc } ~payload:(PStr [ { pstr_desc = Pstr_eval (estring ~loc "-32", []); pstr_loc = loc } ])
  in
  {
    pvb_pat = ppat_var ~loc { txt = var_name; loc };
    pvb_expr = pexp_ident ~loc { txt = Lident var_name; loc };
    pvb_attributes = [ warning_attr ];
    pvb_loc = loc;
  }

let transformation =
  object (self)
    inherit [value_binding list] Ast_traverse.fold_map as super

    method! structure_item item acc =
      match item.pstr_desc with
      (* let%mik/%pcre x = {|some regex|}*)
      | Pstr_extension (({ txt = ("pcre" | "mikmatch") as ext; _ }, PStr [ { pstr_desc = Pstr_value (rec_flag, vbs); _ } ]), _) ->
        let mode = if ext = "pcre" then `Pcre else `Mik in
        let processed_vbs, collected_bindings =
          List.fold_left
            (fun (vbs_acc, bindings_acc) vb ->
              match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
              (* pattern definition - let%mikmatch/%pcre name = {|/regex/|} *)
              | Ppat_var { txt = var_name; loc }, Pexp_constant (Pconst_string (_, _, _)) ->
                let binding = Transformations.transform_let ~mode vb in
                let alias = make_alias_binding ~loc ~var_name in
                alias :: vbs_acc, binding :: bindings_acc
              (* destructuring - let%mikmatch {|/pattern/|} = expr *)
              | Ppat_constant (Pconst_string (pattern_str, _, _)), _ ->
                let new_vb, new_bindings = Transformations.transform_destructuring_let ~mode ~loc:vb.pvb_loc pattern_str vb.pvb_expr in
                new_vb :: vbs_acc, new_bindings @ bindings_acc
              | _ ->
                let binding = Transformations.transform_let ~mode vb in
                binding :: vbs_acc, binding :: bindings_acc)
            ([], acc) vbs
        in

        let new_item = { item with pstr_desc = Pstr_value (rec_flag, List.rev processed_vbs) } in
        new_item, collected_bindings
      (* let x = expression (which might contain %mik/%pcre) *)
      | Pstr_value (rec_flag, vbs) ->
        let processed_vbs, collected_bindings =
          List.fold_left
            (fun (vbs_acc, bindings_acc) vb ->
              match vb.pvb_expr.pexp_desc with
              | Pexp_extension ({ txt = ("pcre" | "mikmatch") as ext; _ }, PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ])
                when match expr.pexp_desc with Pexp_constant (Pconst_string _) -> true | _ -> false ->
                let mode = if ext = "pcre" then `Pcre else `Mik in
                let new_vb = { vb with pvb_expr = expr } in
                let binding = Transformations.transform_let ~mode new_vb in
                let alias =
                  match vb.pvb_pat.ppat_desc with Ppat_var { txt = var_name; loc } -> make_alias_binding ~loc ~var_name | _ -> new_vb
                in
                alias :: vbs_acc, binding :: bindings_acc
              | _ ->
                let new_expr, new_bindings = self#expression vb.pvb_expr bindings_acc in
                let new_vb = { vb with pvb_expr = new_expr } in
                new_vb :: vbs_acc, new_bindings)
            ([], acc) vbs
        in
        let new_item = { item with pstr_desc = Pstr_value (rec_flag, List.rev processed_vbs) } in
        new_item, collected_bindings
      | _ -> super#structure_item item acc

    method! expression e_ext acc =
      let e_ext, acc = super#expression e_ext acc in
      let has_ext_case =
        List.exists
          begin
            fun case -> match case.pc_lhs.ppat_desc with Ppat_extension ({ txt = "pcre" | "mikmatch"; _ }, _) -> true | _ -> false
          end
      in
      match e_ext.pexp_desc with
      (* match%mikmatch/match%pcre and function%mikmatch/function%pcre, mikmatch anchored *)
      | Pexp_extension ({ txt = ("pcre" | "mikmatch") as ext; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
        let mode = if ext = "pcre" then `Pcre else `Mik in
        let loc = e.pexp_loc in
        begin
          match e.pexp_desc with
          | Pexp_function cases ->
            let cases, binding = Transformations.transform_cases ~mode ~loc cases in
            [%expr fun _ppx_regexp_v -> [%e cases]], binding @ acc
          | Pexp_match (e, cases) ->
            let cases, binding = Transformations.transform_cases ~mode ~loc cases in
            ( [%expr
                let _ppx_regexp_v = [%e e] in
                [%e cases]],
              binding @ acc )
          | Pexp_let (rec_flag, vbs, body) ->
            let processed_vbs, new_bindings =
              List.fold_left
                (fun (vbs_acc, bindings_acc) vb ->
                  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
                  | Ppat_constant (Pconst_string (pattern_str, _, _)), _ ->
                    let new_vb, new_bindings = Transformations.transform_destructuring_let ~mode ~loc:vb.pvb_loc pattern_str vb.pvb_expr in
                    new_vb :: vbs_acc, new_bindings @ bindings_acc
                  | _ ->
                    Util.error ~loc
                      "[%%pcre] and [%%mikmatch] only apply to match, function, global let declarations of strings, and let destructuring.")
                ([], []) vbs
            in
            pexp_let ~loc rec_flag (List.rev processed_vbs) body, new_bindings @ acc
          | _ ->
            Util.error ~loc
              "[%%pcre] and [%%mikmatch] only apply to match, function, global let declarations of strings, and let destructuring."
        end
      (* match smth with | {%mikmatch|some regex|} -> ...*)
      | Pexp_match (matched_expr, cases) when has_ext_case cases ->
        Transformations.transform_mixed_match ~loc:e_ext.pexp_loc ~matched_expr cases acc
      | Pexp_function cases when has_ext_case cases -> Transformations.transform_mixed_match ~loc:e_ext.pexp_loc cases acc
      | _ -> e_ext, acc
  end

let dispatch_function_binding ~loc =
  let open Ppxlib in
  let open Ast_builder.Default in
  value_binding ~loc
    ~pat:(ppat_var ~loc { txt = "__ppx_regexp_dispatch"; loc })
    ~expr:
      [%expr
        fun marks handlers _g ->
          let rec loop i =
            if i >= Array.length marks then None
            else if Re.Mark.test _g marks.(i) then (match handlers.(i) _g with Some result -> Some result | None -> loop (i + 1))
            else loop (i + 1)
          in
          loop 0]

let impl str =
  let str, rev_bindings = transformation#structure str [] in
  match rev_bindings with
  | [] -> str
  | _ -> begin
    let loc = match List.hd (List.rev rev_bindings) with { pvb_loc; _ } -> pvb_loc in
    let struct_items =
      rev_bindings
      |> List.rev
      |> List.fold_left
           (fun acc binding -> acc @ [%str let [%p binding.pvb_pat] = [%e binding.pvb_expr]])
           [%str [%%i pstr_value ~loc Nonrecursive [ dispatch_function_binding ~loc ]]]
    in
    let mod_expr = pmod_structure ~loc struct_items in
    [%str open [%m mod_expr]] @ str
  end

let () = Driver.register_transformation ~impl "ppx_regexp"
