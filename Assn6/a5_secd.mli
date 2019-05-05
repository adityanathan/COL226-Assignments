open A5_type
open A4

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = | IntVal of int | BoolVal of bool | Tup of (int * answer list) | CL of string * opcode list * environment

and environment = (string * answer) list

(* opcodes of the secd machine*)
and opcode =
  | LOOKUP of string
  | CLOS of expr * opcode list
  | RET
  | APP
  | PLUS
  | TIMES
  | AND
  | OR
  | BOOL of bool
  | INT of int
  | CMP
  | IFTE
  (*  *)
  | ABS
  | UNARYMINUS
  | NOT
  | MINUS
  | DIV
  | REM
  | EQS
  | GTE
  | LTE
  | GT
  | LT
  | LPAREN
  | RPAREN
  | TUPLE of int
  | PROJ of int * int
  | LET
  | FABS
  | FCALL

exception Variable_not_found

exception Invalid_expression

exception Drop_number_exceeds_list

exception Illegal_Tuple

(* the stack machine *)
val secd_machine: (expr) -> (string * answer) list -> (string * exptype) list -> answer
(* the compiler *)
val compile: expr -> opcode list
