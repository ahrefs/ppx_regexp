
# `%mik` extension

Accepts `mikmatch` syntax, along with some nice to haves.

## Grammar
The grammar accepted by this extensions is the following

```bnf
<main_match_case> ::= "/" <pattern> "/" EOF

<main_let_expr> ::= <pattern> EOF

<pattern> ::= <alt_expr>
            | <alt_expr> ">>>" <func_name> "as" IDENT

<alt_expr> ::= <seq_expr>
             | <seq_expr> "|" <alt_expr>

<seq_expr> ::= <atom_expr>
             | <atom_expr> <seq_expr>

<atom_expr> ::= <basic_atom>
              | <basic_atom> "*"
              | <basic_atom> "+"
              | <basic_atom> "?"
              | <basic_atom> "{" INT (n) "}"             # match n times
              | <basic_atom> "{" INT (n) "-" INT (m) "}" # match at least n times, at most m times

<basic_atom> ::= CHAR_LITERAL
               | STRING_LITERAL
               | EMPTY_STR
               | "_"
               | "^"
               | PREDEFINED_CLASS
               | IDENT
               | "[" <char_set> "]"     # character class
               | "[" "^" <char_set> "]" # negative character class
               | "(" <pattern> ")"
               | "(" IDENT ")"
               | "(" IDENT "as" IDENT ")"
               | "(" IDENT "as" IDENT ":" INT_CONVERTER ")"
               | "(" IDENT "as" IDENT ":" FLOAT_CONVERTER ")"
               | "(" IDENT "as" IDENT ":=" <func_name> ")"
               | "(" <pattern> "as" IDENT ")"
               | "(" <pattern> "as" IDENT ":" INT_CONVERTER ")"
               | "(" <pattern> "as" IDENT ":" FLOAT_CONVERTER ")"
               | "(" <pattern> "as" IDENT ":=" <func_name> ")"

<func_name> ::= IDENT
              | MOD_IDENT # qualified names

<char_set> ::= <char_set_item>
             | <char_set_item> <char_set>

<char_set_item> ::= CHAR_LITERAL
                  | CHAR_LITERAL "-" CHAR_LITERAL
                  | STRING_LITERAL
                  | PREDEFINED_CLASS
                  | IDENT
```

Where `PREDEFINED_CLASS` is one of:
  - **POSIX character classes:** `lower`, `upper`, `alpha`, `digit`, `alnum`, `punct`, `graph`, `print`, `blank`, `space`, `cntrl`, `xdigit`
  - **Control sequences:** `eos` (same as `$`), `eol` (end of string or newline), `bnd` (word boundary `\b`), `bos` (same as `^`), `any` (any character except newline)
  - **Empty string:** `""`, equivalent to `^$` (or `bos eos`)

## Semantics and Examples
### Variable substitution
```ocaml
let%mik re1 = {| "hello" |}
let%mik re2 = {| re1 "world" |}

let do_something = function%mik
  | {|/ ... (re2) ... /|} -> ...
  | _ -> ...

(* will expand to *)
let do_something = function%mik
  | {|/ ... ("hello" "world") ... /|} -> ...
  | _ -> ...
```

### Variable capture
```ocaml
let%mik num = {| digit+ |}

let do_something = function%mik
  | {|/ ... (num as n) ... /|} -> ... (* (n : string) available here *)
  | _ -> ...
```

Values are also available at the guard level:

```ocaml
let%mik num = {| digit+ |}

let do_something = function%mik
  | {|/ ... (num as n) ... /|} when n = 123 -> ...
  | _ -> ...
```

#### Type conversion
It is possible to convert variables to `int` of `float` on the fly:

```ocaml
let%mik num = {| digit+ |}

let do_something = function%mik
  | {|/ 'd' (num as n : int) ... /|} -> ... (* (n : int) available here *)
  | {|/ 'f' (num as n : float) ... /|} -> ... (* (n : float) available here *)
  | _ -> ...
```

It is also possible to pass the variables into any `string -> 'a` function:
```ocaml
let%mik ip = {| (digit{1-3} '.'){3} digit{1-3}|}
let parse_ip = String.split_on_char '.'
let get_ip = function%mik
  | {|/ ... (ip as ip := parse_ip) ... /|} -> ... (* (ip : string list) available here *)
  | _ -> ...

let get_upper_name = function%mik
  | {|/ ... (['a'-'z'] as name := String.uppercase) ... /|} -> ... (* (name : string) available here *)
  | _ -> ...
```

#### Piping to a catch all function

Using the `>>>` syntax extension, you can pipe all bound named variables into a single function, and name its return value.

```ocaml
type example = {
  name : string;
  num : int;
  mode : [ `A | `B | `Default ];
}

let mk_example name num mode = match mode with
  | Some 'a' -> { name; num; mode = `A}
  | Some 'b' -> { name; num; mode = `B}
  | Some _ | None -> { name; num; mode = `Default}

let mk_example_re = function%mik
  | {|/ (['a'-'z'] as name := String.capitalize_ascii) ' ' (digit+ as num : int) ' ' ('a'|'b' as mode)? >>> mk_example as res /|} -> (* (res : example) available here, and all other bound variables *)
  | _ -> ...
```

## Case Insensitive Match

You can use `%mik_i`: `match%mik_i` and `function%mik_i`. (not available at the variable level)

## Alternatives
### Defining variables
You have a choice between:
```ocaml
let%mik re = {|some regex|}
(* and *)
let re = {%mik|some regex|}
```

No `/` delimiters are needed here.

### Matching:
#### `match%mik` and `function%mik`

```ocaml
function%mik
  | {|/ some regex /|} -> ...
  | {|/ another regex /|} -> ...
  ...
  | _ -> ...
```

This match expression will compile all of the REs in the branches into one, and use marks to find which branch was executed.  
Efficient if you have multiple branches.

#### General match/function

```ocaml
function
  | "some string" -> ...
  | {%mik|/ some regex /|} -> ...
  ...
  | "another string" -> ...
  | {%mik_i|/ another regex /|} -> ... (* case insensitive *)
  | _ -> ...
```

This match expression will compile all of the REs **individually**, and test each one in sequence.  
Recommended if you only matching one RE. It is less efficient than the first option for more than one RE, but allows raw string matching.

It keeps all of the features of the previous extension, explored in [Semantics](#Semantics_and_Examples)
