open Ppxlib
open Ast_builder.Default

module Re_comp = struct
  let apply_re_flags ~loc re_expr flags =
    let open Regexp_types in
    let re = if flags.case_insensitive then [%expr Re.no_case [%e re_expr]] else re_expr in
    let re = if flags.anchored then [%expr Re.whole_string [%e re]] else re in
    re

  let compile_single ~loc re flags = [%expr Re.compile [%e apply_re_flags ~loc re flags]]

  let compile_group ~loc res_with_flags =
    let marked = List.map (fun (re, flags) -> [%expr Re.mark [%e apply_re_flags ~loc re flags]]) res_with_flags in
    [%expr
      let a = [%e pexp_array ~loc marked] in
      let marks = Array.map fst a in
      let re = Re.compile (Re.alt (Array.to_list (Array.map snd a))) in
      re, marks]

  let compile ~loc var_name re_array_with_flags =
    let comp_expr =
      match re_array_with_flags with
      | [ (re, flags) ] ->
        [%expr
          let re = [%e compile_single ~loc re flags] in
          re, [||]]
      | _ -> compile_group ~loc re_array_with_flags
    in
    value_binding ~loc ~pat:(ppat_var ~loc { txt = var_name; loc }) ~expr:comp_expr
end

module Bindings = struct
  let wrap_group_expr ~loc ~mustG iG offG conv =
    let eG = match iG with None -> [%expr Re.Group.get _g 0] | Some iG -> [%expr Re.Group.get _g [%e eint ~loc (offG + iG + 1)]] in
    let eG =
      match conv with
      | None -> eG
      | Some Regexp_types.Int -> [%expr int_of_string [%e eG]]
      | Some Float -> [%expr float_of_string [%e eG]]
      | Some (Func func_name) ->
        let func_ident = pexp_ident ~loc { txt = Util.extract_qualified_name func_name; loc } in
        [%expr [%e func_ident] [%e eG]]
      | Some (Pipe_all_func _) -> Util.error ~loc ">>> not allowed inside patterns"
    in
    if mustG then eG else [%expr try Some [%e eG] with Not_found -> None]

  let rec wrap_group_bindings ~loc ~captured_acc rhs offG = function
    | [] -> rhs
    | [ (varG, _, Some (Regexp_types.Pipe_all_func func_name), _) ] ->
      let func_ident = pexp_ident ~loc { txt = Util.extract_qualified_name func_name; loc } in
      let captured = List.rev captured_acc in
      let func_app = List.fold_left (fun expr arg -> [%expr [%e expr] [%e arg]]) func_ident captured in
      [%expr
        let [%p ppat_var ~loc varG] = [%e func_app] in
        [%e rhs]]
    | (varG, iG, conv, mustG) :: bs ->
      let eG = wrap_group_expr ~loc ~mustG iG offG conv in
      let pat = ppat_var ~loc varG in
      let pat_ident = pexp_ident ~loc @@ { txt = Util.extract_qualified_name varG.txt; loc = varG.loc } in
      [%expr
        let [%p pat] = [%e eG] in
        [%e wrap_group_bindings ~loc ~captured_acc:(pat_ident :: captured_acc) rhs offG bs]]
end

module Regexp = struct
  open Regexp_types
  include Regexp

  let bindings =
    let rec recurse must_match (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      match e'.Location.txt with
      | Code _ -> fun acc -> acc
      | Seq es -> Util.List.fold (recurse must_match) es
      | Alt es -> Util.List.fold (recurse false) es
      | Opt e -> recurse false e
      | Repeat ({ Location.txt = i, _; _ }, e) -> recurse (must_match && i > 0) e
      | Nongreedy e -> recurse must_match e
      | Caseless e -> recurse must_match e
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mikmatch."
      | Capture_as (idr, conv, e) -> fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Named_subs (idr, None, conv, e) | Named_subs (_, Some idr, conv, e) ->
        fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Unnamed_subs (_, e) -> recurse must_match e
      | Pipe_all (res, func, e) ->
        fun (nG, bs) ->
          let nG', inner_bs = recurse must_match e (nG, []) in
          nG', ((res, None, Some (Pipe_all_func func), must_match) :: inner_bs) @ bs
      | Call _ -> Util.error ~loc "(&...) is not implemented for %%pcre and %%mikmatch."
    in
    function { Location.txt = Capture_as (idr, conv, e); _ } -> recurse true e (0, [ idr, None, conv, true ]) | e -> recurse true e (0, [])

  let to_re_expr ~ctx =
    let rec recurse ~ctx (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      match e'.Location.txt with
      | Code s -> [%expr Re.Perl.re [%e estring ~loc s]]
      | Seq es -> [%expr Re.seq [%e elist ~loc (List.map (recurse ~ctx) es)]]
      | Alt es -> [%expr Re.alt [%e elist ~loc (List.map (recurse ~ctx) es)]]
      | Opt e -> [%expr Re.opt [%e recurse ~ctx e]]
      | Repeat ({ Location.txt = i, j_opt; _ }, e) ->
        let e_i = eint ~loc i in
        let e_j = match j_opt with None -> [%expr None] | Some j -> [%expr Some [%e eint ~loc j]] in
        [%expr Re.repn [%e recurse ~ctx e] [%e e_i] [%e e_j]]
      | Nongreedy e -> [%expr Re.non_greedy [%e recurse ~ctx e]]
      | Caseless e -> [%expr Re.no_case [%e recurse ~ctx e]]
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mikmatch."
      | Capture_as (_, _, e) -> [%expr Re.group [%e recurse ~ctx e]]
      | Named_subs (idr, _, _, _) ->
        let content = get_substitution ~loc ~ctx idr in
        [%expr Re.group [%e recurse ~ctx content]]
      | Unnamed_subs (idr, _) ->
        let content = get_substitution ~loc ~ctx idr in
        recurse ~ctx content
      | Pipe_all (_, _, e) -> recurse ~ctx e
      | Call _ -> Util.error ~loc "Call is not allowed for %%pcre and %%mikmatch."
    and get_substitution ~loc ~ctx idr =
      let var_name = idr.txt in
      match Util.Ctx.find var_name ctx with
      | Some value -> value
      | None -> Util.error ~loc "Variable '%s' not found. %%pcre and %%mikmatch only support global let bindings for substitution." var_name
    in
    function { Location.txt = Capture_as (_, _, e); _ } -> recurse ~ctx e | e -> recurse ~ctx e

  let rec squash_codes (e : _ Location.loc) : _ Location.loc =
    let open Location in
    let rec combine (nodes : _ Location.loc list) =
      match nodes with
      | [] -> []
      | { Location.txt = Code s1; loc = loc1 } :: { Location.txt = Code s2; loc = loc2 } :: rest ->
        let combined_loc =
          if loc1 = Location.none || loc2 = Location.none then Location.none
          else Location.{ loc_start = loc1.loc_start; loc_end = loc2.loc_end; loc_ghost = false }
        in
        combine ({ Location.txt = Code (s1 ^ s2); loc = combined_loc } :: rest)
      | node :: rest -> node :: combine rest
    in
    match e.txt with
    | Code _ -> e
    | Seq es ->
      let es = List.map squash_codes es in
      { e with txt = Seq (combine es) }
    | Alt es ->
      let es = List.map squash_codes es in
      { e with txt = Alt es }
    | Opt e' -> { e with txt = Opt (squash_codes e') }
    | Repeat (range, e') -> { e with txt = Repeat (range, squash_codes e') }
    | Nongreedy e' -> { e with txt = Nongreedy (squash_codes e') }
    | Caseless e' -> { e with txt = Caseless (squash_codes e') }
    | Capture e' -> { e with txt = Capture (squash_codes e') }
    | Capture_as (name, j, e') -> { e with txt = Capture_as (name, j, squash_codes e') }
    | Named_subs (name, j, conv, e') -> { e with txt = Named_subs (name, j, conv, squash_codes e') }
    | Unnamed_subs (name, e') -> { e with txt = Unnamed_subs (name, squash_codes e') }
    | Pipe_all (r, f, e') -> { e with txt = Pipe_all (r, f, squash_codes e') }
    | Call _ -> e
end

module Parser = struct
  let get_parser mode target = match mode with `Pcre -> Regexp.parse_exn ~target | `Mik -> Regexp.parse_mik_exn ~target

  let run ~parser ~ctx s =
    let r, flags = parser s in
    let r = Regexp.squash_codes r in
    let nG, bs = Regexp.bindings r in
    let re = Regexp.to_re_expr ~ctx r in
    re, bs, nG, flags
end

let make_default_rhs ~mode ~loc = function
  | [] ->
    let open Lexing in
    let pos = loc.Location.loc_start in
    let pos_end = loc.Location.loc_end in
    let lnum = eint ~loc pos.pos_lnum in
    let lnum_end = eint ~loc pos_end.pos_lnum in
    let e0 = estring ~loc pos.pos_fname in
    let e2 = eint ~loc (pos.pos_cnum - pos.pos_bol) in
    let e2_start = eint ~loc (pos.pos_cnum - pos.pos_bol) in
    let e2_end = eint ~loc (pos_end.pos_cnum - pos_end.pos_bol) in
    begin
      match mode with
      | `Pcre ->
        let e = [%expr raise (Match_failure ([%e e0], [%e lnum], [%e e2]))] in
        Util.warn ~loc "A universal case is recommended for %%pcre." e
      | `Mik ->
        let str = [%expr Printf.sprintf "File %s, lines %d-%d, characters %d-%d: String did not match any of the mikmatch regexes."] in
        [%expr raise (Failure ([%e str] [%e e0] [%e lnum] [%e lnum_end] [%e e2_start] [%e e2_end]))]
    end
  | default_cases ->
    let transformed =
      List.map
        (fun case ->
          match case.pc_lhs.ppat_desc with
          | Ppat_var var ->
            {
              case with
              pc_lhs = ppat_any ~loc;
              pc_rhs =
                [%expr
                  let [%p ppat_var ~loc var] = _ppx_regexp_v in
                  [%e case.pc_rhs]];
            }
          | _ -> case)
        default_cases
    in
    begin
      match transformed with
      | [ { pc_lhs = { ppat_desc = Ppat_any; _ }; pc_guard = None; pc_rhs; _ } ] -> pc_rhs
      | _ -> pexp_match ~loc [%expr _ppx_regexp_v] transformed
    end

let build_exec_match ~loc ~re_var ~continue_next ~on_match =
  [%expr match Re.exec_opt (fst [%e re_var]) _ppx_regexp_v with None -> [%e continue_next] | Some _g -> [%e on_match]]

(* Transformations *)

let transform_let ~mode ~ctx =
  let parser = Parser.get_parser mode `Let in
  List.map
    begin
      fun vb ->
        match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
        | Ppat_var { txt = var_name; loc }, Pexp_constant (Pconst_string (value, _, _)) ->
          if Util.check_unbounded_recursion ~mode var_name value then Util.error ~loc "Unbounded recursion detected!"
          else begin
            let string_loc = vb.pvb_expr.pexp_loc in
            let pos = string_loc.loc_start in
            let pos = { pos with pos_cnum = pos.pos_cnum + 2 } in
            let parsed, _flags = parser ~pos value in
            Hashtbl.replace ctx var_name parsed;
            let warning_attr =
              attribute ~loc ~name:{ txt = "ocaml.warning"; loc }
                ~payload:(PStr [ { pstr_desc = Pstr_eval (estring ~loc "-32", []); pstr_loc = loc } ])
            in
            { vb with pvb_attributes = warning_attr :: vb.pvb_attributes }
          end
        | _ -> vb
    end

let transform_cases ~mode ~loc ~ctx cases =
  let partition_cases cases =
    let rec partition pattern_cases = function
      | [] -> List.rev pattern_cases, []
      | ({ pc_lhs = { ppat_desc = Ppat_any | Ppat_var _; _ }; _ } as case) :: rest -> List.rev pattern_cases, case :: rest
      | case :: rest -> partition (case :: pattern_cases) rest
    in
    partition [] cases
  in

  let parse_pattern ~mode ~ctx case =
    Ast_pattern.(parse (pstring __')) case.pc_lhs.ppat_loc case.pc_lhs (fun { txt = re_src; loc = { loc_start; loc_end; _ } } ->
      let re_offset = (loc_end.pos_cnum - loc_start.pos_cnum - String.length re_src) / 2 in
      let pos = { loc_start with pos_cnum = loc_start.pos_cnum + re_offset; pos_lnum = loc_end.pos_lnum } in
      let parser = Parser.get_parser mode `Match ~pos in
      let re, bs, nG, flags = Parser.run ~parser ~ctx re_src in
      let re_str = Pprintast.string_of_expression re in
      re, re_str, nG, bs, case.pc_rhs, case.pc_guard, flags)
  in

  let create_compilation_groups parsed_cases =
    let can_merge_into_group (_, re_str, _, _, _, guard, flags) group =
      match guard with
      | None ->
        (* no guard: all must be guard-free *)
        List.for_all (fun (_, _, _, _, _, g', _) -> g' = None) group
      | Some _ ->
        (* has guard: needs exact (RE string, flags) match *)
        List.exists (fun (_, re_str', _, _, _, _, f') -> re_str = re_str' && flags = f') group
    in

    let rec group acc current_group = function
      | [] -> if current_group = [] then List.rev acc else List.rev (current_group :: acc)
      | case :: rest ->
        if current_group = [] || can_merge_into_group case current_group then group acc (case :: current_group) rest
        else group (List.rev current_group :: acc) [ case ] rest
    in
    group [] [] parsed_cases
  in

  let process_compilation_group group_idx cases_in_group =
    (* deduplicate identical (pattern, flags) combinations *)
    let deduplicate_patterns cases =
      let add_case patterns (re, re_str, nG, bs, rhs, guard, flags) =
        let key = re_str, flags in
        let handlers = nG, bs, rhs, guard in
        let re_data = re, flags in
        match List.assoc_opt key patterns with
        | Some (re_data, existing) -> (key, (re_data, handlers :: existing)) :: List.remove_assoc key patterns
        | None -> (key, (re_data, [ handlers ])) :: patterns
      in
      List.fold_left add_case [] cases |> List.map (fun ((_re_str, _flags), (re_data, handlers)) -> re_data, List.rev handlers) |> List.rev
    in

    let add_offsets patterns =
      let rec calc acc offset = function
        | [] -> List.rev acc
        | ((re, flags), handlers) :: rest ->
          let max_captures = handlers |> List.map (fun (n, _, _, _) -> n) |> List.fold_left max 0 in
          calc (((re, flags), handlers, offset) :: acc) (offset + max_captures) rest
      in
      calc [] 0 patterns
    in

    let create_handler idx ((_, _), handlers, offset) =
      let name = Printf.sprintf "_group%d_case_%d" group_idx idx in

      let rec chain_guards = function
        | [] -> [%expr None]
        | (_, bs, rhs, None) :: _ -> [%expr Some [%e Bindings.wrap_group_bindings ~captured_acc:[] ~loc rhs offset (List.rev bs)]]
        | (_, bs, rhs, Some guard) :: rest ->
          let guarded = [%expr if [%e guard] then Some [%e rhs] else [%e chain_guards rest]] in
          Bindings.wrap_group_bindings ~captured_acc:[] ~loc guarded offset (List.rev bs)
      in

      name, [%expr fun _g -> [%e chain_guards (List.rev handlers)]]
    in

    let unique_patterns = deduplicate_patterns cases_in_group in
    let patterns_with_offsets = add_offsets unique_patterns in
    let handlers = List.mapi create_handler patterns_with_offsets in
    let patterns_and_flags = List.map (fun ((re, flags), _, _) -> re, flags) patterns_with_offsets in

    Util.fresh_var (), patterns_and_flags, handlers
  in

  let generate_code compilation_groups processed_groups default_rhs =
    let re_bindings =
      List.map (fun (var_name, patterns_and_flags, _) -> Re_comp.compile ~loc var_name patterns_and_flags) processed_groups
    in

    let handler_bindings =
      processed_groups
      |> List.concat_map (fun (_, _, handlers) ->
           handlers |> List.map (fun (name, body) -> value_binding ~loc ~pat:(ppat_var ~loc { txt = name; loc }) ~expr:body))
    in

    let build_match_cascade () =
      let make_case idx ((var_name, patterns, handlers), original_group) =
        let re_var = pexp_ident ~loc { txt = Lident var_name; loc } in
        let continue = [%expr __ppx_regexp_try_next ([%e eint ~loc idx] + 1)] in
        let has_guards = List.exists (fun (_, _, _, _, _, g, _) -> g <> None) original_group in
        let is_single = match patterns with [ _ ] -> true | _ -> false in

        let on_match =
          if is_single then (
            let handler = pexp_ident ~loc { txt = Lident (fst (List.hd handlers)); loc } in
            [%expr match [%e handler] _g with Some result -> result | None -> [%e continue]])
          else (
            let handler_array = handlers |> List.map (fun (name, _) -> pexp_ident ~loc { txt = Lident name; loc }) |> pexp_array ~loc in
            let dispatch = [%expr __ppx_regexp_dispatch (snd [%e re_var]) [%e handler_array] _g] in
            if has_guards then [%expr match [%e dispatch] with Some result -> result | None -> [%e continue]]
            else [%expr match [%e dispatch] with Some result -> result | None -> assert false])
        in

        case
          ~lhs:(ppat_constant ~loc (Pconst_integer (string_of_int idx, None)))
          ~guard:None
          ~rhs:(build_exec_match ~loc ~re_var ~continue_next:continue ~on_match)
      in

      let cases = List.mapi make_case (List.combine processed_groups compilation_groups) in
      let default = case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:default_rhs in

      [%expr
        let rec __ppx_regexp_try_next group_idx = [%e pexp_match ~loc [%expr group_idx] (cases @ [ default ])] in
        __ppx_regexp_try_next 0]
    in

    let cascade = build_match_cascade () in
    if handler_bindings = [] then cascade, re_bindings else pexp_let ~loc Nonrecursive handler_bindings cascade, re_bindings
  in

  let pattern_cases, default_cases = partition_cases cases in
  let default_rhs = make_default_rhs ~mode ~loc default_cases in

  pattern_cases |> List.map (parse_pattern ~mode ~ctx) |> create_compilation_groups |> fun groups ->
  let processed = List.mapi process_compilation_group groups in
  generate_code groups processed default_rhs

let transform_mixed_match ~loc ~ctx ?matched_expr cases acc =
  let aux case =
    match case.pc_lhs.ppat_desc with
    | Ppat_extension
        ( { txt = ("pcre" | "mikmatch") as ext; _ },
          PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (pat, str_loc, _)); _ }, _); _ } ] ) ->
      let pos = str_loc.loc_start in
      let mode = if "pcre" = ext then `Pcre else `Mik in
      let parser = Parser.get_parser mode `Match ~pos in
      let re, bs, nG, flags = Parser.run ~parser ~ctx pat in
      `Ext (re, nG, bs, case.pc_rhs, case.pc_guard, flags)
    | _ -> `Regular case
  in

  let prepared_cases = List.map aux cases in
  let has_ext = List.exists (function `Ext _ -> true | _ -> false) prepared_cases in

  if not has_ext then (match matched_expr with None -> pexp_function ~loc cases, acc | Some m -> pexp_match ~loc m cases, acc)
  else begin
    let compilations =
      prepared_cases
      |> List.mapi (fun i case ->
           match case with
           | `Ext (re, _, _, _, _, flags) ->
             let comp_var = Util.fresh_var () in
             let comp_expr = Re_comp.compile_single ~loc re flags in
             let binding = value_binding ~loc ~pat:(ppat_var ~loc { txt = comp_var; loc }) ~expr:comp_expr in
             Some (i, comp_var, binding)
           | _ -> None)
      |> List.filter_map (fun x -> x)
    in

    let bindings = List.map (fun (_, _, b) -> b) compilations in

    let rec build_ordered_match input_var case_idx cases comps =
      match cases, comps with
      | [], _ -> [%expr raise (Match_failure ("", 0, 0))]
      | `Regular case :: rest, _ ->
        [%expr
          match [%e input_var] with
          | [%p case.pc_lhs] when [%e Option.value case.pc_guard ~default:[%expr true]] -> [%e case.pc_rhs]
          | _ -> [%e build_ordered_match input_var (case_idx + 1) rest comps]]
      | `Ext (_, _, bs, rhs, guard, _) :: rest, (idx, comp_var, _) :: rest_comps when idx = case_idx ->
        let comp_ident = pexp_ident ~loc { txt = Lident comp_var; loc } in
        [%expr
          match Re.exec_opt [%e comp_ident] [%e input_var] with
          | Some _g ->
            [%e
              let bs = List.rev bs in
              match guard with
              | None -> Bindings.wrap_group_bindings ~captured_acc:[] ~loc rhs 0 bs
              | Some g ->
                let guarded_rhs = [%expr if [%e g] then [%e rhs] else [%e build_ordered_match input_var (case_idx + 1) rest rest_comps]] in
                Bindings.wrap_group_bindings ~captured_acc:[] ~loc guarded_rhs 0 bs]
          | None -> [%e build_ordered_match input_var (case_idx + 1) rest rest_comps]]
      | `Ext _ :: rest, _ -> build_ordered_match input_var (case_idx + 1) rest comps
    in

    let match_body = build_ordered_match [%expr _ppx_regexp_v] 0 prepared_cases compilations in
    let match_expr =
      match matched_expr with
      | None -> [%expr fun _ppx_regexp_v -> [%e match_body]]
      | Some m ->
        [%expr
          let _ppx_regexp_v = [%e m] in
          [%e match_body]]
    in
    match_expr, bindings @ acc
  end
