open A5_type

(* Function 'hastype' takes a set of type assumptions G represented as a list
of tuples of form (variable name, type), an expression and an expression
type, and returns if the expression has the claimed type under the given
assumptions. *)
val hastype : ((string * exptype) list) -> expr -> exptype -> bool

val type_infer : ((string * exptype) list) -> expr -> exptype

exception Variable_not_found

exception Drop_number_exceeds_list

exception Type_infer_invalid
