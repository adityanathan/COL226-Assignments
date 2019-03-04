open A0

(* abstract syntax  *)
type  exptree =  Done | N of int (* Integer constant *)
  | B of bool (* Boolean constant *)
  | Var of string (* variable *)
  | Conjunction of exptree * exptree (* binary operators on booleans /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  | Equals of exptree * exptree      (* comparison operations on integers *)
  | GreaterTE of exptree * exptree   (* comparison operations on integers *)
  | LessTE of exptree * exptree      (* comparison operations on integers *)
  | GreaterT of exptree * exptree    (* comparison operations on integers *)
  | LessT of exptree * exptree       (* comparison operations on integers *)
  | InParen of exptree               (* expressions using parenthesis *)
  | IfThenElse of exptree * exptree * exptree (* a conditional expression *)
  | Tuple of int * exptree           (* creating n-tuples (n >= 0) *)
  | Project of int * exptree         (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Plus of exptree * exptree        (* binary operators on integers *)
  | Minus of exptree * exptree       (* binary operators on integers *)
  | Mult of exptree * exptree        (* binary operators on integers *)
  | Div of exptree * exptree         (* binary operators on integers *)
  | Rem of exptree * exptree         (* binary operators on integers *)
  | Nega of exptree       (* unary operators on booleans *)
  | Abs of exptree        (* unary operators on integers *)

(* opcodes of the stack machine *)
type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS
  | EQS | GTE | LTE | GT | LT | PAREN
  | CONST of bool | CONJ | DISJ | NOT |
  | IFTE | TUPLE of int | PROJ of int

(* the definitional interpreter *)
val eval : exptree -> int
val stackmc: (bigint list) -> (opcode list) -> bigint

val compile: exptree -> opcode list
