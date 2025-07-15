open Ppxlib
open Ast_builder.Default

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
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mik."
      | Capture_as (idr, conv, e) ->
        fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Named_subs (idr, None, conv, e) | Named_subs (_, Some idr, conv, e) ->
        fun (nG, bs) -> recurse must_match e (nG + 1, (idr, Some nG, conv, must_match) :: bs)
      | Unnamed_subs (_, e) -> recurse must_match e
      | Pipe_all (res, func, e) ->
        fun (nG, bs) ->
          let nG', inner_bs = recurse must_match e (nG, []) in
          nG', ((res, None, Some (Pipe_all_func func), must_match) :: inner_bs) @ bs
      | Call _ -> Util.error ~loc "(&...) is not implemented for %%pcre and %%mik."
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
          match Util.Ctx.find var_name ctx with
          | Some value -> value
          | None ->
            Util.error ~loc
              "Variable '%s' not found. %%pcre and %%mik only support global let bindings for substitution." var_name
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
      | Capture _ -> Util.error ~loc "Unnamed capture is not allowed for %%pcre and %%mik."
      | Capture_as (_, _, e) -> "(" ^ recurse p_alt e ^ ")"
      | Named_subs (idr, _, _, _) ->
        let content = get_parsed idr in
        "(" ^ recurse p_alt content ^ ")"
      | Unnamed_subs (idr, _) ->
        let content = get_parsed idr in
        recurse p_alt content
      | Pipe_all (_, _, e) -> recurse p_alt e
      | Call _ -> Util.error ~loc "(&...) is not implemented for %%pcre and %%mik."
    in
    function { Location.txt = Capture_as (_, _, e); _ } -> recurse 0 e | e -> recurse 0 e
end

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
        let func_ident = pexp_ident ~loc { txt = Util.extract_qualified_name func_name; loc } in
        [%expr [%e func_ident] [%e eG]]
      | Some (Regexp_types.Pipe_all_func _) ->
        Util.error ~loc "No >>> allowed inside patterns" (* parser makes sure this is never the case *)
    in
    let eG = if mustG then eG else [%expr try Some [%e eG] with Not_found -> None] in

    let pat = ppat_var ~loc varG in
    let pat_ident = pexp_ident ~loc @@ { txt = Util.extract_qualified_name varG.txt; loc = varG.loc } in
    [%expr
      let [%p pat] = [%e eG] in
      [%e wrap_group_bindings ~loc ~captured_acc:(pat_ident :: captured_acc) rhs offG bs]]

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
  |> List.map (fun (g, cases) -> g, cases)

let rec separate_defaults acc = function
  | [] -> List.rev acc, []
  | ({ pc_lhs = { ppat_desc = Ppat_any; _ }; _ } as case) :: rest -> acc, case :: rest
  | ({ pc_lhs = { ppat_desc = Ppat_var _; _ }; _ } as case) :: rest -> acc, case :: rest
  | case :: rest -> separate_defaults (case :: acc) rest

let extract_bindings ~(parser : ?pos:position -> string -> string Regexp_types.t) ~ctx ~pos s =
  let r = parser ~pos s in
  let nG, bs = Regexp.bindings r in
  let re_str = Regexp.to_string ~ctx r in
  let loc = Location.none in
  estring ~loc re_str, bs, nG

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
    pexp_match ~loc [%expr _ppx_regexp_v] transformed

let transform_let ~mode ~ctx =
  let parser = match mode with `Pcre -> Regexp.parse_exn ~target:`Let | `Mik -> Regexp.parse_mik_exn ~target:`Let in
  List.map
    begin
      fun vb ->
        match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
        | Ppat_var { txt = var_name; loc }, Pexp_constant (Pconst_string (value, _, _)) ->
          if Util.check_unbounded_recursion ~mode var_name value then Util.error ~loc "Unbounded recursion detected!"
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
  let cases, default_cases = separate_defaults [] cases in
  let default_rhs = make_default_rhs ~loc default_cases in

  let processed_cases = List.rev_map aux cases in
  let res = pexp_array ~loc @@ List.map (fun (re, _, _, _, _) -> re) processed_cases in

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

  let var = Util.fresh_var () in
  let re_binding = value_binding ~loc ~pat:(ppat_var ~loc { txt = var; loc }) ~expr:comp in
  let e_comp = pexp_ident ~loc { txt = Lident var; loc } in

  let rec handle_cases i offG = function
    | [] -> default_rhs
    | (_, nG, bs, rhs, guard) :: cases ->
      let bs = List.rev bs in
      let handled_cases = handle_cases (i + 1) (offG + nG) cases in
      [%expr
        if Re.Mark.test _g (snd [%e e_comp]).([%e eint ~loc i]) then
          [%e
            let wrapped_with_guard =
              match guard with
              | None -> rhs
              | Some guard_expr -> [%expr if [%e guard_expr] then [%e rhs] else [%e handled_cases]]
            in
            wrap_group_bindings ~captured_acc:[] ~loc wrapped_with_guard offG bs]
        else [%e handled_cases]]
  in

  let match_expr =
    [%expr
      match Re.exec_opt (fst [%e e_comp]) _ppx_regexp_v with
      | None -> [%e default_rhs]
      | Some _g -> [%e handle_cases 0 0 processed_cases]]
  in

  ( [%expr
      let _ppx_regexp_v = [%e pexp_ident ~loc { txt = Lident "_ppx_regexp_v"; loc }] in
      [%e match_expr]],
    [ re_binding ] )

(* processes each case individually instead of combining them into one RE *)
let transform_mixed_match ~loc ~ctx ?matched_expr cases acc =
  let aux case =
    match case.pc_lhs.ppat_desc with
    | Ppat_extension
        ( { txt = "mik"; _ },
          PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (pat, str_loc, _)); _ }, _); _ } ]
        ) ->
      let pos = str_loc.loc_start in
      let parser = Regexp.parse_mik_exn ~target:`Match in
      let re, bs, nG = extract_bindings ~parser ~pos ~ctx pat in
      `Mik (re, nG, bs, case.pc_rhs, case.pc_guard)
    | _ -> `Regular case
  in

  let prepared_cases = List.map aux cases in

  let has_mik = List.exists (function `Mik _ -> true | _ -> false) prepared_cases in

  if not has_mik then begin
    match matched_expr with None -> pexp_function ~loc cases, acc | Some m -> pexp_match ~loc m cases, acc
  end
  else begin
    let mik_compilations =
      List.mapi
        begin
          fun i case ->
            match case with
            | `Mik (re, _, _, _, _) ->
              let comp_var = Util.fresh_var () in
              let comp_expr = [%expr Re.compile (Re.Perl.re [%e re])] in
              let binding = value_binding ~loc ~pat:(ppat_var ~loc { txt = comp_var; loc }) ~expr:comp_expr in
              Some (i, comp_var, binding)
            | _ -> None
        end
        prepared_cases
      |> List.filter_map (fun x -> x)
    in

    let bindings = List.map (fun (_, _, b) -> b) mik_compilations in

    let rec build_ordered_match input_var case_idx cases mik_comps =
      match cases, mik_comps with
      | [], _ ->
        (* should not happen if original had catch-all *)
        [%expr raise (Match_failure ("", 0, 0))]
      | `Regular case :: rest, _ ->
        [%expr
          match [%e input_var] with
          | [%p case.pc_lhs] when [%e Option.value case.pc_guard ~default:[%expr true]] -> [%e case.pc_rhs]
          | _ -> [%e build_ordered_match input_var (case_idx + 1) rest mik_comps]]
      | `Mik (_, _, bs, rhs, guard) :: rest, (idx, comp_var, _) :: rest_comps when idx = case_idx ->
        let comp_ident = pexp_ident ~loc { txt = Lident comp_var; loc } in
        [%expr
          match Re.exec_opt [%e comp_ident] [%e input_var] with
          | Some _g ->
            [%e
              let bs = List.rev bs in
              (* let body = wrap_group_bindings ~captured_acc:[] ~loc rhs 0 bs in *)
              match guard with
              | None -> wrap_group_bindings ~captured_acc:[] ~loc rhs 0 bs
              | Some g ->
                let guarded_rhs =
                  [%expr if [%e g] then [%e rhs] else [%e build_ordered_match input_var (case_idx + 1) rest rest_comps]]
                in
                wrap_group_bindings ~captured_acc:[] ~loc guarded_rhs 0 bs]
          | None -> [%e build_ordered_match input_var (case_idx + 1) rest rest_comps]]
      | `Mik _ :: rest, _ ->
        (* shouldn't happen if indices are correct *)
        build_ordered_match input_var (case_idx + 1) rest mik_comps
    in

    let match_body = build_ordered_match [%expr _ppx_regexp_v] 0 prepared_cases mik_compilations in

    let match_expr =
      let init =
        match matched_expr with
        | None -> [%expr fun _ppx_regexp_v -> [%e match_body]]
        | Some m ->
          [%expr
            let _ppx_regexp_v = [%e m] in
            [%e match_body]]
      in
      List.fold_left
        begin
          fun expr binding ->
            [%expr
              let [%p binding.pvb_pat] = [%e binding.pvb_expr] in
              [%e expr]]
        end
        init bindings
    in

    match_expr, bindings @ acc
  end
