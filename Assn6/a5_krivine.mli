open A5_type
open A4

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = IntVal of int | BoolVal of bool | CL of expr * environment | Tup of int * answer list

and environment = (string * answer) list

exception Illegal_Tuple

exception Variable_not_found

exception Invalid_expression

exception Projection_Index_Out_of_bounds

(* the krivine machine *)
val krivine_machine: (expr) -> ((string * expr) list) -> ((string * exptype) list) -> answer
