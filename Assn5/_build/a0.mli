(* Integers *)
type bigint = sign * int list
  and sign = Neg | NonNeg

(* Arithmetic operations:  *)
(* Addition *)
val add: bigint -> bigint -> bigint
(* Multiplication *)
val mult: bigint -> bigint -> bigint
(* Subtraction *)
val sub: bigint -> bigint -> bigint
(* Quotient *)
val div: bigint -> bigint -> bigint
(* Remainder *)
val rem: bigint -> bigint -> bigint
(* Unary negation *)
val minus: bigint -> bigint
(* Absolute value *)
val abs: bigint -> bigint

(* Comparison operations:  *)
(* Equal *)
val eq: bigint -> bigint -> bool
(* Greater_than. *)
val gt:  bigint -> bigint -> bool
(* Less_than. *)
val lt:  bigint -> bigint -> bool
(* Great_or_equal. *)
val geq:  bigint -> bigint -> bool
(* Less_or_equal.  *)
val leq:  bigint -> bigint -> bool

(* Functions to present the result in the form of a string. *)
val print_num: bigint -> string

(* Conversion functions from OCaml int to bigint. *)
val mk_big:  int -> bigint
