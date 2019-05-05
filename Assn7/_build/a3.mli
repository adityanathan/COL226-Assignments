type token =
  | CALL
  | DISPLAY
  | RETURN
  | EXIT
  | COMMA
  | LP
  | RP
  | EQ
  | COLON
  | EOF
  | INT of (int)
  | ID of (string)

val parser :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A7.command
