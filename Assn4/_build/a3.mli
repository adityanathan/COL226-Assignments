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
  | INT of (int)
  | ID of (string)
  | BOOL of (bool)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptree
