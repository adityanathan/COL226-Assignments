type token =
  | COMMA
  | TILDA
  | LP
  | RP
  | IF
  | THEN
  | ELSE
  | FI
  | DELIMITER
  | EOF
  | EQ
  | GT
  | LT
  | ABS
  | EXP
  | DIV
  | REM
  | TIMES
  | PLUS
  | MINUS
  | DISJ
  | CONJ
  | NOT
  | PROJ
  | LET
  | IN
  | END
  | BACKSLASH
  | DOT
  | DEF
  | SEMICOLON
  | PARALLEL
  | LOCAL
  | COLON
  | TINT
  | TBOOL
  | TUNIT
  | ARROW
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)

val def_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.definition
val exp_parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptree
