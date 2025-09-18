# PPX for Working with Regular Expressions

This repo provides a PPX providing regular expression-based routing:

- `ppx_regexp_extended` maps to [re][] with the conventional last-match extraction
  into `string` and `string option`. Two syntaxes for regular expressions available:
  - `pcre`: The syntax of regular PCRE expressions
  - `mikmatch`: Mimics the syntax of the [mikmatch](https://mjambon.github.io/mjambon2016/mikmatch-manual.html) tool

This syntax extension turns:
```ocaml
function%pcre
| {|re1|} -> e1
...
| {|reN|} -> eN
| _ -> e0
```
(or `function%mikmatch`) into suitable invocations of the [Re library][re], and similar for `match%pcre`/`match%mikmatch`.

It also accepts:
```ocaml
let%pcre var = {| some regex |}
(* and *)
let%mikmatch var = {| some regex |}
```

### `%mikmatch`

Full [%mikmatch guide](./MIKMATCH.md).

#### Quick Links
- [Variable capture](./MIK.md#variable-capture)
- [Type conversion](./MIK.md#type-conversion)
- [Different extensions](./MIK.md#alternatives)

#### Motivational Examples

URL parsing:
```ocaml
let parse s =
  let (scheme, first) =
    match s.[4] with
    | ':' -> `Http, 7
    | 's' -> `Https, 8
    | _ -> failwith "parse"
  in
  let last = String.index_from s first '/' in
  let host = String.slice s ~first ~last in
  let (host,port) =
    match Stre.splitc host ':' with
    | exception _ -> host, default_port scheme
    | (host,port) -> host, int_of_string port
  in
  ...

(* in mikmatch: *)

let parse s =
  match%mikmatch s with
  | {|/ "http" ('s' as https)? "://" ([^ '/' ':']+ as host) (":" (digit+ as port : int))? '/'? (_* as rest) /|} ->
      let scheme = match https with Some _ -> `Https | None -> `Http in
      let port = match port with Some p -> p | None -> default_port scheme in
      ...
  | _ -> failwith "parse"

```

```ocaml
let rex =
  let origins = "csv|pdf|html|xlsv|xml"
  Re2.create_exn (sprintf {|^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z)(?:\.(\d+))?\.(%s)\.(\d+)\.(\d+)$|} origins)

let of_string s =
  try
    let m = Re2.first_match_exn rex s in
    let start = Re2.Match.get_exn ~sub:(`Index 1) m |> U.strptime "%Y-%m-%dT%H:%M:%S%z" |> U.timegm in
    let shard = int_of_string (Re2.Match.get_exn ~sub:(`Index 2) m) in
    let origin = origin_of_string (Re2.Match.get_exn ~sub:(`Index 3) m) in
    let partition = int_of_string (Re2.Match.get_exn ~sub:(`Index 4) m) in
    let worker = int_of_string (Re2.Match.get_exn ~sub:(`Index 5) m) in
    { start; shard; origin; partition; worker }
  with _ -> invalid_arg (sprintf "error: %s" s)

(* in mikmatch: *)

let%mikmatch origins = {| "csv" | "pdf" | "html" | "xlsv" | "xml" |}

let of_string s =
  match%mikmatch s with
  | {|/ (digit{4} '-' digit{2} '-' digit{2} 'T' digit{2} ':' digit{2} ':' digit{2} 'Z' as timestamp)
      ('.' (digit+ as shard : int))? 
      '.' (origins as origin := origin_of_string)
      '.' (digit+ as partition : int)
      '.' (digit+ as worker : int) /|} ->
      let start = U.strptime "%Y-%m-%dT%H:%M:%S%z" timestamp |> U.timegm in
      let shard = match shard with Some s -> s | None -> 0 in
      { start; shard; origin; partition; worker }
  | _ -> invalid_arg (sprintf "error: %s" s)

```

### `%pcre`

The patterns are plain strings of the form accepted by `Re.Pcre`, with the following additions:

  - `(?<var>...)` defines a group and binds whatever it matches as `var`.
    The type of `var` will be `string` if the match is guaranteed given that
    the whole pattern matches, and `string option` if the variable is bound
    to or nested below an optionally matched group.

  - `(?&var)` refers to any identifier bound to a typed regular expression of type `'a Re.t`
    and binds whatever it matches as `var`.
    The type of `var` will be the same as `(?<var>...)`.

  - `(?&name:var>)` same as above but binds whatever it matches as `name`. (shortcut for `(?<v>(?&qname))`)
    The type of `name` will be the same as `(?<var>...)`.

  - `?<var>` at the start of a pattern binds group 0 as `var : string`.
    This may not be the full string if the pattern is unanchored.

A variable is allowed for the universal case and is bound to the matched
string.

### Type definitions from patterns
You can generate record types from regex patterns:

```ocaml
type url = {%mikmatch|
  (("http" | "https") as scheme) "://"
  ((alnum+ ('.' alnum+)*) as host)
  (':' (digit+ as port : int))?
  ('/' ([^'?' '#']* as path))?
  ('?' ([^'#']* as query))?
  ('#' (any* as fragment))?
|}
```
This generates:
- A record type with fields for each named capture
- `parse_url : string -> url option` - parses strings into the type
- `pp_url : Format.formatter -> url -> unit` - pretty-prints back to string format

> [!WARNING]
> When printing, repetitions will be executed the minimum required amount of times.  
> `*` prints nothing

#### Smart reconstruction
The pretty-printer intelligently handles alternations and optional fields:
```ocaml
let%mikmatch date_format = {| digit{4} '-' digit{2} '-' digit{2} ' ' digit{2} ':' digit{2} ':' digit{2} |}

type date = {%mikmatch| (date_format as date) |}

type mode =
  [ `A
  | `B
  | `Other
  ]

let mk_mode = function "a" -> `A | "b" -> `B | _ -> `Other
let pp_mode fmt mode = Format.fprintf fmt @@ match mode with `A -> "a" | `B -> "b" | `Other -> "other"

type log = {%mikmatch| 
  (date_format as date)
  " [" (upper+ as level) "]"
  ((" pid=" (digit+ as pid : int))? | (" name=" ([a-z]+ as name))?)
  ' '{2-3}
  ('a'|'b'|"other" as mode := mk_mode : mode)
  ": "
  (any+ as message)
|}

let input = "2025-06-13 12:42:12 [INFO] pid=123  a: something happened" in
match parse_log input with
| Some log ->
  (* Prints: "2025-06-13 12:42:12 [INFO] pid=123  a: something happened" *)
  Format.printf "%a@." pp_log log;
  
  (* Change from pid to name variant *)
  let log' = { log with pid = None; name = Some "server" } in
  (* Prints: "2025-06-13 12:42:12 [INFO] name=server  a: something happened" *)
  Format.printf "%a@." pp_log log'
```
The pretty-printer detects which alternation branch to use based on field population - if `pid` is `Some _`, it prints the `pid` branch; if `name` is `Some _`, it prints the `name` branch.

##### Type conversions and custom parsers
- For function application you are required to pass the return type.
- If the return type is itself an application (e.g. `string list`), then you must provide a type alias.  
- The type must have an associated `pp` function. (Notice, in the example, the `mode` type and its associated functions)

### Example

The following prints out times and hosts for SMTP connections to the Postfix daemon:

#### `%pcre`
```ocaml
(* Link with re, re.pcre, lwt, lwt.unix.
   Preprocess with ppx_regexp_extended.
   Adjust to your OS. *)

open Lwt.Infix

let check_line =
  (function%pcre
   | {|(?<t>.*:\d\d) .* postfix/smtpd\[[0-9]+\]: connect from (?<host>[a-z0-9.-]+)|} ->
      Lwt_io.printlf "%s %s" t host
   | _ ->
      Lwt.return_unit)

let () = Lwt_main.run begin
  Lwt_io.printl "SMTP connections from:" >>= fun () ->
  Lwt_stream.iter_s check_line (Lwt_io.lines_of_file "/var/log/syslog")
end
```

#### `%mikmatch`
```ocaml
(* Link with re, re.pcre, lwt, lwt.unix.
   Preprocess with ppx_regexp_extended.
   Adjust to your OS. *)

open Lwt.Infix

let%mikmatch host = {| [a-z0-9.-]+ |}

let check_line =
  (function%mikmatch
   | {|/ (any* ':' digit digit as t) ' ' (any*) ' ' "postfix/smtpd" '[' digit+ ']' ": connect from " (host) /|} ->
      Lwt_io.printlf "%s %s" t host
   | _ ->
      Lwt.return_unit)

let () = Lwt_main.run begin
  Lwt_io.printl "SMTP connections from:" >>= fun () ->
  Lwt_stream.iter_s check_line (Lwt_io.lines_of_file "/var/log/syslog")
end
```

## Limitations

### No Exhaustiveness Check

The syntax extension will always warn if no catch-all case is provided.  No
exhaustiveness check is attempted.  Doing it right would require
reimplementing full regular expression parsing and an algorithm which would
ideally produce a counter-example.

## Bug Reports

The processor is currently new and not well tested.  Please break it and
file bug reports in the GitHub issue tracker.  Any exception raised by
generated code except for `Match_failure` is a bug.

[re]: https://github.com/ocaml/ocaml-re
