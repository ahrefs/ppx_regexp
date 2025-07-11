%{
open Regexp_types

let nonepsilon = function { Location.txt = Seq []; _ } -> false | _ -> true

let simplify_seq ~loc es =
  match List.filter nonepsilon es with
  | [ e ] -> e
  | es -> mkloc (Seq es) loc

let simplify_alt es =
  match es with
  | [ e ] -> e.Location.txt
  | es -> Alt es

let make_loc startpos endpos =
  Location.{
    loc_start = startpos;
    loc_end = endpos;
    loc_ghost = false;
  }

let wrap_loc startpos endpos x =
  Location.{
    txt = x;
    loc = make_loc startpos endpos;
  }

let to_pcre_regex str startpos endpos =
  wrap_loc startpos endpos (Code str)

let syntax_error msg startpos endpos =
  let loc = make_loc startpos endpos in
  raise @@ PError (loc, msg)

let missing_error what startpos endpos =
  syntax_error (Printf.sprintf "Missing %s" what) startpos endpos

let unclosed_error what startpos endpos =
  syntax_error (Printf.sprintf "Unclosed %s" what) startpos endpos
%}

%token <string> CHAR_LITERAL STRING_LITERAL IDENT MOD_IDENT PREDEFINED_CLASS
%token <int> INT
%token SLASH LPAREN RPAREN LBRACKET RBRACKET CARET LBRACE RBRACE
%token DASH BAR STAR PLUS QUESTION UNDERSCORE COLON EQUAL AS
%token INT_CONVERTER FLOAT_CONVERTER EOF

%start <string t> main_match_case
%start <string t> main_let_expr
%start <string t> pattern

/* operator precedence from lowest to highest */
%right AS                             /* priority 3: capture */
%left BAR                             /* priority 2: alternation */
%left seq                             /* priority 1: sequence (implicit) */
%nonassoc STAR PLUS QUESTION LBRACE   /* priority 0: repetition */

%%

main_match_case:
  | SLASH p = pattern SLASH EOF { p }
  | SLASH pattern EOF { unclosed_error "pattern (missing closing '/')" $startpos($1) $endpos }
  | SLASH error { syntax_error "Invalid pattern after opening slash" $startpos($2) $endpos($2) }
  | error { syntax_error "Expected pattern to start with '/'" $startpos($1) $endpos($1) }

main_let_expr:
  | p = pattern EOF { p }

pattern:
  | alt_expr { $1 }
  | { missing_error "pattern expression" $startpos $endpos }

alt_expr:
  | seq_expr { $1 }
  | seq_expr BAR alt_expr {
      let loc = make_loc $startpos $endpos in
      mkloc (simplify_alt [$1; $3]) loc
    }
  | seq_expr BAR { missing_error "expression after '|'" $startpos($2) $endpos }
  | BAR { missing_error "expression before '|'" $startpos $endpos }

seq_expr:
  | atom_expr { $1 }
  | atom_expr seq_expr %prec seq {
      let loc = make_loc $startpos $endpos in
      simplify_seq ~loc [$1; $2]
    }

atom_expr:
  | basic_atom { $1 }
  | basic_atom STAR {
      let repeat_loc = wrap_loc $startpos($2) $endpos($2) (0, None) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom PLUS {
      let repeat_loc = wrap_loc $startpos($2) $endpos($2) (1, None) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom QUESTION {
      wrap_loc $startpos $endpos (Opt $1)
    }
  | basic_atom LBRACE n = INT RBRACE {
      let repeat_loc = wrap_loc $startpos($2) $endpos($4) (n, Some n) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom LBRACE min = INT DASH max = INT RBRACE {
      let repeat_loc = wrap_loc $startpos($2) $endpos($6) (min, Some max) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  | basic_atom LBRACE min = INT DASH RBRACE {
      let repeat_loc = wrap_loc $startpos($2) $endpos($5) (min, None) in
      wrap_loc $startpos $endpos (Repeat (repeat_loc, $1))
    }
  (* error cases for repetition *)
  | basic_atom LBRACE RBRACE { missing_error "repetition count" $startpos($2) $endpos($3) }
  | basic_atom LBRACE INT DASH DASH { syntax_error "Invalid repetition range" $startpos($4) $endpos($5) }
  | basic_atom LBRACE INT { unclosed_error "repetition (missing '}')" $startpos($2) $endpos }
  | basic_atom LBRACE error { syntax_error "Invalid repetition syntax" $startpos($2) $endpos }
  (* error cases for missing atoms before operators *)
  | STAR { missing_error "expression before '*'" $startpos $endpos }
  | PLUS { missing_error "expression before '+'" $startpos $endpos }
  | QUESTION { missing_error "expression before '?'" $startpos $endpos }

basic_atom:
  | CHAR_LITERAL {
      to_pcre_regex $1 $startpos $endpos
    }
  | STRING_LITERAL {
      to_pcre_regex $1 $startpos $endpos
    }
  | UNDERSCORE {
      to_pcre_regex "." $startpos $endpos
    }
  | PREDEFINED_CLASS {
      to_pcre_regex $1 $startpos $endpos
    }
  | IDENT {
      let ident_loc = wrap_loc $startpos $endpos $1 in
      let pattern_node = to_pcre_regex $1 $startpos $endpos in
      wrap_loc $startpos $endpos (Unnamed_subs (ident_loc, pattern_node))
    }

  | LBRACKET char_set RBRACKET {
      let set_str = "[" ^ $2 ^ "]" in
      to_pcre_regex set_str $startpos $endpos
    }
  | LBRACKET CARET char_set RBRACKET {
      let set_str = "[^" ^ $3 ^ "]" in
      to_pcre_regex set_str $startpos $endpos
    }
  | LBRACKET RBRACKET { missing_error "character set content" $startpos $endpos }
  | LBRACKET CARET RBRACKET { missing_error "character set content after '^'" $startpos $endpos }
  | LBRACKET char_set { unclosed_error "character set (missing ']')" $startpos($1) $endpos }
  | LBRACKET error { syntax_error "Invalid character set" $startpos $endpos }

  | LPAREN pattern RPAREN {
      $2
    }
  | LPAREN RPAREN { missing_error "pattern inside parentheses" $startpos $endpos }
  | LPAREN pattern EOF? { unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($2) }

  | LPAREN IDENT RPAREN {
      let ident_loc = wrap_loc $startpos($2) $endpos($2) $2 in
      let pattern_node = to_pcre_regex $2 $startpos($2) $endpos($2) in
      wrap_loc $startpos $endpos (Named_subs (ident_loc, None, None, pattern_node))
    }
  | LPAREN IDENT AS RPAREN { missing_error "name after 'as'" $startpos($3) $endpos($4) }
  | LPAREN IDENT AS name = IDENT RPAREN {
      let ident_loc = wrap_loc $startpos($2) $endpos($2) $2 in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let pattern_node = to_pcre_regex $2 $startpos($2) $endpos($2) in
      wrap_loc $startpos $endpos (Named_subs (ident_loc, Some name_loc, None, pattern_node))
    }
  | LPAREN IDENT AS IDENT COLON RPAREN {
      missing_error "type converter after ':'" $startpos($5) $endpos($6) 
    }
  | LPAREN IDENT AS name = IDENT COLON INT_CONVERTER RPAREN {
      let ident_loc = wrap_loc $startpos($2) $endpos($2) $2 in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let pattern_node = to_pcre_regex $2 $startpos($2) $endpos($2) in
      wrap_loc $startpos $endpos (Named_subs (ident_loc, Some name_loc, Some Int, pattern_node))
    }
  | LPAREN IDENT AS name = IDENT COLON FLOAT_CONVERTER RPAREN {
      let ident_loc = wrap_loc $startpos($2) $endpos($2) $2 in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let pattern_node = to_pcre_regex $2 $startpos($2) $endpos($2) in
      wrap_loc $startpos $endpos (Named_subs (ident_loc, Some name_loc, Some Float, pattern_node))
    }
  | LPAREN IDENT AS name = IDENT COLON EQUAL func = MOD_IDENT RPAREN
  | LPAREN IDENT AS name = IDENT COLON EQUAL func = IDENT RPAREN {
      let ident_loc = wrap_loc $startpos($2) $endpos($2) $2 in
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      let pattern_node = to_pcre_regex $2 $startpos($2) $endpos($2) in
      wrap_loc $startpos $endpos (Named_subs (ident_loc, Some name_loc, Some (Func func), pattern_node))
    }
  | LPAREN IDENT AS IDENT EOF? { 
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($4)
    }
  | LPAREN IDENT AS IDENT COLON INT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }
  | LPAREN IDENT AS IDENT COLON FLOAT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }

  | LPAREN pattern AS RPAREN { missing_error "capture name after 'as'" $startpos($3) $endpos($4) }
  | LPAREN pattern AS name = IDENT RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, None, $2))
    }
  | LPAREN pattern AS IDENT COLON RPAREN {
      missing_error "type converter after ':'" $startpos($5) $endpos($6)
    }
  | LPAREN pattern AS IDENT COLON EQUAL RPAREN {
      missing_error "function name after ':='" $startpos($5) $endpos($6)
    }
  | LPAREN pattern AS name = IDENT COLON INT_CONVERTER RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Int, $2))
    }
  | LPAREN pattern AS name = IDENT COLON FLOAT_CONVERTER RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some Float, $2))
    }
  | LPAREN pattern AS name = IDENT COLON EQUAL func = MOD_IDENT RPAREN
  | LPAREN pattern AS name = IDENT COLON EQUAL func = IDENT RPAREN {
      let name_loc = wrap_loc $startpos(name) $endpos(name) name in
      wrap_loc $startpos $endpos (Capture_as (name_loc, Some (Func func), $2))
    }
  | LPAREN pattern AS IDENT EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($4)
    }
  | LPAREN pattern AS IDENT COLON INT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }
  | LPAREN pattern AS IDENT COLON FLOAT_CONVERTER EOF? {
      unclosed_error "parentheses (missing ')')" $startpos($1) $endpos($6)
    }

  | LPAREN error { syntax_error "Invalid expression in parentheses" $startpos($2) $endpos }

char_set:
  | char_set_item { $1 }
  | char_set_item char_set { $1 ^ $2 }

char_set_item:
  | CHAR_LITERAL { $1 }
  | CHAR_LITERAL DASH CHAR_LITERAL { $1 ^ "-" ^ $3 }
  | CHAR_LITERAL DASH { missing_error "character after '-' in range" $startpos($2) $endpos }
  | STRING_LITERAL { $1 }
  | PREDEFINED_CLASS { $1 }
  | IDENT { $1 }
