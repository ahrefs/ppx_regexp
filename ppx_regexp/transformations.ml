open Ppxlib
open Ast_builder.Default

module Re_comp = struct
  let apply_re_opts ~loc re_expr opts =
    let rec apply re = function
      | [] -> re
      | `Caseless :: rest -> apply [%expr Re.no_case [%e re]] rest
      | `Anchored :: rest -> apply [%expr Re.whole_string [%e re]] rest
    in
    apply re_expr opts

  let compile_single ~loc re opts = [%expr Re.compile [%e apply_re_opts ~loc re opts]]

  let compile_marked_group ~loc res opts =
    let res_with_opts = List.map (fun re -> [%expr Re.mark [%e apply_re_opts ~loc re opts]]) res in
    [%expr
      let a = [%e pexp_array ~loc res_with_opts] in
      let marks = Array.map fst a in
      let re = Re.compile (Re.alt (Array.to_list (Array.map snd a))) in
      re, marks]

  let create_re_binding ~loc var_name re_array opts =
    let is_single = match re_array.pexp_desc with Pexp_array [ _ ] -> true | _ -> false in
    let comp_expr =
      if is_single then (
        match re_array.pexp_desc with
        | Pexp_array [ re ] ->
          [%expr
            let re = [%e compile_single ~loc re opts] in
            re, [||]]
        | _ -> assert false)
      else (match re_array.pexp_desc with Pexp_array res -> compile_marked_group ~loc res opts | _ -> assert false)
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
      | Some Regexp_types.Float -> [%expr float_of_string [%e eG]]
      | Some (Regexp_types.Func func_name) ->
        let func_ident = pexp_ident ~loc { txt = Util.extract_qualified_name func_name; loc } in
        [%expr [%e func_ident] [%e eG]]
      | Some (Regexp_types.Pipe_all_func _) -> Util.error ~loc "No >>> allowed inside patterns"
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

  let to_string ~ctx =
    let p_alt, p_seq, p_suffix, p_atom = 0, 1, 2, 3 in
    let delimit_if b s = if b then "(?:" ^ s ^ ")" else s in
    let get_parsed ~loc idr =
      let var_name = idr.txt in
      match Util.Ctx.find var_name ctx with
      | Some value -> value
      | None -> Util.error ~loc "Variable '%s' not found. %%pcre and %%mikmatch only support global let bindings for substitution." var_name
    in
    let rec recurse p (e' : _ Location.loc) =
      let loc = e'.Location.loc in
      match e'.Location.txt with
      | Code s -> s
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
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mikmatch."
      | Capture_as (_, _, e) -> "(" ^ recurse p_alt e ^ ")"
      | Named_subs (idr, _, _, _) -> "(" ^ recurse p_alt (get_parsed ~loc idr) ^ ")"
      | Unnamed_subs (idr, _) -> recurse p_atom (get_parsed ~loc idr)
      | Pipe_all (_, _, e) -> recurse p_alt e
      | Call _ -> Util.error ~loc "(&...) is not implemented for %%pcre and %%mikmatch."
    in
    function { Location.txt = Capture_as (_, _, e); _ } -> recurse 0 e | e -> recurse 0 e
end

module Parser = struct
  let get_parser mode target = match mode with `Pcre -> Regexp.parse_exn ~target | `Mik -> Regexp.parse_mik_exn ~target

  let extract_bindings ~parser ~ctx s =
    let r = parser s in
    let nG, bs = Regexp.bindings r in
    let re = Regexp.to_re_expr ~ctx r in
    re, bs, nG

  let parse_mode_from_ext ext =
    let mode = if String.starts_with ~prefix:"pcre" ext then `Pcre else `Mik in
    let base_opts = match mode with `Pcre -> [] | `Mik -> Util.mikmatch_default_opts in
    let opts = if String.ends_with ~suffix:"_i" ext then `Caseless :: base_opts else base_opts in
    mode, opts
end

let rec separate_defaults acc = function
  | [] -> List.rev acc, []
  | ({ pc_lhs = { ppat_desc = Ppat_any; _ }; _ } as case) :: rest -> acc, case :: rest
  | ({ pc_lhs = { ppat_desc = Ppat_var _; _ }; _ } as case) :: rest -> acc, case :: rest
  | case :: rest -> separate_defaults (case :: acc) rest

let make_default_rhs ~loc = function
  | [] ->
    let open Lexing in
    let pos = loc.Location.loc_start in
    let e0 = estring ~loc pos.pos_fname in
    let e1 = eint ~loc pos.pos_lnum in
    let e2 = eint ~loc (pos.pos_cnum - pos.pos_bol) in
    let e = [%expr raise (Match_failure ([%e e0], [%e e1], [%e e2]))] in
    Util.warn ~loc "A universal case is recommended." e
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

let extract_re_from_case ~mode ~ctx case =
  Ast_pattern.(parse (pstring __')) case.pc_lhs.ppat_loc case.pc_lhs (fun { txt = re_src; loc = { loc_start; loc_end; _ } } ->
    let re_offset = (loc_end.pos_cnum - loc_start.pos_cnum - String.length re_src) / 2 in
    let pos = { loc_start with pos_cnum = loc_start.pos_cnum + re_offset; pos_lnum = loc_end.pos_lnum } in
    let parser = Parser.get_parser mode `Match ~pos in
    let re, bs, nG = Parser.extract_bindings ~parser ~ctx re_src in
    re, nG, bs, case.pc_rhs, case.pc_guard)

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

let transform_cases ~mode ~opts ~loc ~ctx cases =
  let cases, default_cases = separate_defaults [] cases in
  let default_rhs = make_default_rhs ~loc default_cases in
  let processed_cases = List.map (extract_re_from_case ~mode ~ctx) cases in

  let group_by_guard_and_re cases =
    let rec group acc current_group = function
      | [] -> if current_group = [] then acc else current_group :: acc
      | ((re, _, _, _, guard) as case) :: rest ->
        (match current_group with
        | [] -> group acc [ case ] rest
        | cases_in_group ->
          let can_merge =
            match guard with
            | None -> List.for_all (fun (_, _, _, _, g) -> g = None) cases_in_group
            | Some _ -> List.exists (fun (re', _, _, _, _) -> re = re') cases_in_group
          in
          if can_merge then group acc (case :: current_group) rest else group (List.rev current_group :: acc) [ case ] rest)
    in
    group [] [] cases
  in

  let compile_group group_idx group_cases =
    let group_by_pattern cases =
      let add_case groups (re, nG, bs, rhs, guard) =
        let rec update = function
          | [] -> [ re, [ nG, bs, rhs, guard ] ]
          | (re', cs) :: rest when re = re' -> (re', (nG, bs, rhs, guard) :: cs) :: rest
          | g :: rest -> g :: update rest
        in
        update groups
      in
      List.fold_left add_case [] cases |> List.rev
    in

    let calculate_offsets pattern_groups =
      let rec calc acc offset = function
        | [] -> List.rev acc
        | (re, handlers) :: rest ->
          let max_captures = handlers |> List.map (fun (n, _, _, _) -> n) |> List.fold_left max 0 in
          calc ((re, handlers, offset) :: acc) (offset + max_captures) rest
      in
      calc [] 0 pattern_groups
    in

    let create_handler handler_name case_handlers offset =
      let rec make_guard_chain = function
        | [] -> [%expr None]
        | (_, bs, rhs, None) :: _ -> [%expr Some [%e Bindings.wrap_group_bindings ~captured_acc:[] ~loc rhs offset (List.rev bs)]]
        | (_, bs, rhs, Some guard_expr) :: rest ->
          let guarded = [%expr if [%e guard_expr] then Some [%e rhs] else [%e make_guard_chain rest]] in
          Bindings.wrap_group_bindings ~captured_acc:[] ~loc guarded offset (List.rev bs)
      in
      handler_name, [%expr fun _g -> [%e make_guard_chain (List.rev case_handlers)]]
    in

    let pattern_groups = group_by_pattern group_cases in
    let patterns_with_offsets = calculate_offsets pattern_groups in
    let re_array = patterns_with_offsets |> List.map (fun (re, _, _) -> re) |> pexp_array ~loc in
    let handlers =
      List.mapi
        (fun i (_, case_handlers, offset) -> create_handler (Printf.sprintf "_group%d_case_%d" group_idx i) case_handlers offset)
        patterns_with_offsets
    in
    re_array, handlers
  in

  let case_groups = group_by_guard_and_re processed_cases in

  let compiled_groups =
    List.mapi
      begin
        fun i group_cases ->
          let re_var_name = Util.fresh_var () in
          let re_array, handlers = compile_group i group_cases in
          re_var_name, re_array, handlers
      end
      case_groups
  in

  let re_bindings = List.map (fun (var_name, re_array, _) -> Re_comp.create_re_binding ~loc var_name re_array opts) compiled_groups in

  let handler_bindings =
    List.concat_map
      begin
        fun (_, _, handlers) ->
          handlers |> List.map (fun (name, body) -> value_binding ~loc ~pat:(ppat_var ~loc { txt = name; loc }) ~expr:body)
      end
      compiled_groups
  in

  let build_match_cascade groups =
    let groups_with_info =
      List.mapi
        begin
          fun i (re_var_name, re_array, handlers) ->
            let group_cases = List.nth case_groups i in
            let has_guards = List.exists (fun (_, _, _, _, g) -> g <> None) group_cases in
            let is_single = match re_array.pexp_desc with Pexp_array [ _ ] -> true | _ -> false in
            i, re_var_name, handlers, has_guards, is_single
        end
        groups
    in

    let match_cases =
      List.map
        begin
          fun (idx, re_var_name, handlers, has_guards, is_single) ->
            let re_var = pexp_ident ~loc { txt = Lident re_var_name; loc } in
            let continue_next = [%expr __ppx_regexp_try_next ([%e eint ~loc idx] + 1)] in

            let on_match =
              if is_single then begin
                let handler = pexp_ident ~loc { txt = Lident (fst (List.hd handlers)); loc } in
                [%expr match [%e handler] _g with Some result -> result | None -> [%e continue_next]]
              end
              else begin
                let handlers_array =
                  handlers |> List.map (fun (name, _) -> pexp_ident ~loc { txt = Lident name; loc }) |> pexp_array ~loc
                in
                let dispatch = [%expr __ppx_regexp_dispatch (snd [%e re_var]) [%e handlers_array] _g] in
                if has_guards then [%expr match [%e dispatch] with Some result -> result | None -> [%e continue_next]]
                else [%expr match [%e dispatch] with Some result -> result | None -> assert false]
              end
            in

            let match_expr = build_exec_match ~loc ~re_var ~continue_next ~on_match in
            case ~lhs:(ppat_constant ~loc (Pconst_integer (string_of_int idx, None))) ~guard:None ~rhs:match_expr
        end
        groups_with_info
    in

    let default_case = case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:default_rhs in
    [%expr
      let rec __ppx_regexp_try_next group_idx = [%e pexp_match ~loc [%expr group_idx] (match_cases @ [ default_case ])] in
      __ppx_regexp_try_next 0]
  in

  let match_cascade = build_match_cascade compiled_groups in
  let match_expr = if handler_bindings = [] then match_cascade else pexp_let ~loc Nonrecursive handler_bindings match_cascade in
  match_expr, re_bindings

let transform_mixed_match ~loc ~ctx ?matched_expr cases acc =
  let aux case =
    match case.pc_lhs.ppat_desc with
    | Ppat_extension
        ( { txt = ("pcre" | "mikmatch" | "pcre_i" | "mikmatch_i") as ext; _ },
          PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (pat, str_loc, _)); _ }, _); _ } ] ) ->
      let pos = str_loc.loc_start in
      let mode, opts = Parser.parse_mode_from_ext ext in
      let parser = Parser.get_parser mode `Match ~pos in
      let re, bs, nG = Parser.extract_bindings ~parser ~ctx pat in
      `Ext (opts, re, nG, bs, case.pc_rhs, case.pc_guard)
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
           | `Ext (opts, re, _, _, _, _) ->
             let comp_var = Util.fresh_var () in
             let comp_expr = Re_comp.compile_single ~loc re opts in
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
      | `Ext (_, _, _, bs, rhs, guard) :: rest, (idx, comp_var, _) :: rest_comps when idx = case_idx ->
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
