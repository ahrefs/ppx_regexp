{
open Mik_parser
exception Error of string

let new_line lexbuf =
  Lexing.new_line lexbuf

let predefined_classes = [
  ("lower", {|[[:lower:]]|});
  ("upper", {|[[:upper:]]|});
  ("alpha", {|[[:alpha:]]|});
  ("digit", {|[[:digit:]]|});
  ("alnum", {|[[:alnum:]]|});
  ("punct", {|[[:punct:]]|});
  ("graph", {|[[:graph:]]|});
  ("print", {|[[:print:]]|});
  ("blank", {|[[:blank:]]|});
  ("cntrl", {|[[:cntrl:]]|});
  ("xdigit", {|[[:xdigit:]]|});
  ("space", {|[[:space:]]|});
  ("word", {|[[:word:]]|});
  ("eos", {|$|});
  ("eol", {|$|[\n]|});
  ("bnd", {|\b|});
  ("bos", {|^|});
  ("any", {|[^\n]|});
]

let escape_char = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | '\\' -> '\\'
  | '\'' -> '\''
  | '"' -> '"'
  | c -> c

let escape_special = function
  | '(' -> {|\(|}
  | ')' -> {|\)|}
  | '[' -> {|\[|}
  | ']' -> {|\]|}
  | '{' -> {|\{|}
  | '}' -> {|\}|}
  | '.' -> {|\.|}
  | '*' -> {|\*|}
  | '+' -> {|\+|}
  | '?' -> {|\?|}
  | '^' -> {|\^|}
  | '$' -> {|\$|}
  | '|' -> {|\||}
  | c -> String.make 1 c
}

let whitespace = [' ' '\t' '\r']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase
let digit = ['0'-'9']
let ident = (lowercase | '_') (alpha | digit | '_' | '\'')*
let module_name = uppercase (alpha | digit | '_' | '\'')*
let module_ident = module_name ('.' (module_name | ident))*

rule token = parse
  | [' ' '\t' '\r']+ { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | '/' { SLASH }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '^' { CARET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '-' { DASH }
  | '|' { BAR }
  | '*' { STAR }
  | '+' { PLUS }
  | '?' { QUESTION }
  | '_' { UNDERSCORE }
  | ':' { COLON }
  | '=' { EQUAL }
  | "as" { AS }
  | "int" { INT_CONVERTER }
  | "float" { FLOAT_CONVERTER }
  | digit+ as n { INT (int_of_string n) }
  | module_ident as id { MOD_IDENT id }
  | ident as id {
      match List.assoc_opt id predefined_classes with
      | Some pcre_class -> PREDEFINED_CLASS pcre_class
      | None -> IDENT id
    }
  | '\'' { char_literal (Buffer.create 16) lexbuf }
  | '"' { string_literal (Buffer.create 16) lexbuf }
  | eof { EOF }
  | _ as c { raise (Error ("Unexpected character: " ^ String.make 1 c)) }

and char_literal buf = parse
  | '\\' (_ as c) { 
      Buffer.add_char buf (escape_char c);
      char_literal buf lexbuf
    }
  | '\'' { CHAR_LITERAL (Buffer.contents buf) }
  | ('(' as c) | (')' as c) {
       Buffer.add_string buf (escape_special c);
       char_literal buf lexbuf
     }
  | ('{' as c) | ('}' as c) {
       Buffer.add_string buf (escape_special c);
       char_literal buf lexbuf
     }
  | ('[' as c) | (']' as c) {
       Buffer.add_string buf (escape_special c);
       char_literal buf lexbuf
     }
  | ('.' as c) | ('*' as c) | ('+' as c) | ('?' as c) | ('^' as c) | ('$' as c) | ('|' as c) {
       Buffer.add_string buf (escape_special c);
       char_literal buf lexbuf
     }
  | _ as c {
      Buffer.add_char buf c;
      char_literal buf lexbuf
    }
  | eof { raise (Error "Unterminated character literal") }

and string_literal buf = parse
  | '\\' (_ as c) {
      Buffer.add_char buf (escape_char c);
      string_literal buf lexbuf
    }
  | '"' { STRING_LITERAL (Buffer.contents buf) }
  | _ as c {
      Buffer.add_char buf c;
      string_literal buf lexbuf
    }
  | eof { raise (Error "Unterminated string literal") }
