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
  | GTA
  | LTA
  | ABS
  | EXP
  | DIV
  | MOD
  | MUL
  | PLUS
  | MINUS
  | AND
  | OR
  | NOT
  | INT of (int)
  | ID of (string)
  | BOOL of (bool)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptree
