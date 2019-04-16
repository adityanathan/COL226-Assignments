open A1

(* Function 'hastype' takes a set of type assumptions G represented as a list
of tuples of form (variable name, type), an expression and an expression
type, and returns if the expression has the claimed type under the given
assumptions. *)
val hastype : ((string * exptype) list) -> exptree -> exptype -> bool

(* Function 'yields' takes a set of type assumptions G, a definition d and
another set of type assumptions G', and decides whether under the given
assumptions G, the definition d yields the type assumptions G' or not. *)
val yields: ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool
