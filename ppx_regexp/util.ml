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

let extract_qualified_name n =
  match String.split_on_char '.' n with
  | [] -> assert false
  | [ name ] -> Lident name
  | module_path ->
    let rec build_longident = function
      | [] -> assert false
      | [ name ] -> Lident name
      | module_name :: rest -> Ldot (build_longident rest, module_name)
    in
    build_longident (List.rev module_path)

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

let pp_conv = function
  | None -> "NONE"
  | Some Regexp_types.Int -> "INT"
  | Some Regexp_types.Float -> "FLOAT"
  | Some (Regexp_types.Func func_name) -> Format.sprintf "FUNC_NAME: %s." func_name
  | Some (Regexp_types.Pipe_all_func func_name) -> Format.sprintf "PIPE_FUNC_NAME: %s" func_name

(* debugging *)
let rec debug_re indent (ast : string Regexp_types.t) =
  let spaces = String.make indent ' ' in
  match ast.txt with
  | Code s -> Printf.printf "%sCode(%S)\n" spaces s
  | Seq exprs ->
    Printf.printf "%sSeq[\n" spaces;
    List.iter (debug_re (indent + 2)) exprs;
    Printf.printf "%s]\n" spaces
  | Alt exprs ->
    Printf.printf "%sAlt[\n" spaces;
    List.iter (debug_re (indent + 2)) exprs;
    Printf.printf "%s]\n" spaces
  | Opt expr ->
    Printf.printf "%sOpt(\n" spaces;
    debug_re (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Repeat (_, expr) ->
    Printf.printf "%sRepeat(\n" spaces;
    debug_re (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Capture expr ->
    Printf.printf "%sCapture(\n" spaces;
    debug_re (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Capture_as (name, conv, expr) ->
    Printf.printf "%sCapture_as(%s, %s,\n" spaces (pp_conv conv) name.txt;
    debug_re (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Named_subs (name, alias, conv, expr) ->
    Printf.printf "%sNamed_subs(%s, %s, %s,\n" spaces name.txt (pp_conv conv) (match alias with Some a -> a.txt | None -> "None");
    debug_re (indent + 2) expr;
    Printf.printf "%s)\n" spaces
  | Call longident -> Printf.printf "%sCall(%s)\n" spaces (match longident.txt with Longident.Lident s -> s | _ -> "complex")
  | _ -> Printf.printf "%sOther\n" spaces
