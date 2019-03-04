(* Assignment 0. A Bigint package
This is a preliminary assignment which will be used in following assignments.  In this assignment, you will write in OCaml a BIGNUM package  where you will implement arithmetic for arbitrarily large numbers, using lists of digits to implement an integer.
*)

module type BigInt = sig
  type bigint = sign * int list
    and sign = Neg | NonNeg
  (* Representational invariant of the elements of the int list:
    - the elements of the int list are between 0 and 9
    - presented most significant digit first
    - there are no unnecessary leading zeros. *)

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

  (* Define suitable exceptions when an operation is not defined. *)
end

(* Instructions for submission:
#1 Your submission should have one file named structure_a0.ml which implements the interface as below.
  open Signature_a0
  module A0 : BigInt = struct
   (* Your code goes here *)
  end

#2 Your code won't compile if all the functions declared in the signature_a0.mli are not implemented in the structure_a0.ml. In case you decide not to implement a function, but want your code to compile and run.
 - Mention this in the comments
 - Add "exception Not_implemented" in the structure_a0 (NOT signature_a0.mli)
 - Provide a dummy implementation (for those functions which you aren't implementing) which raises an exception Not_implemented
E.g.: In case you aren't implementing add
  exception Not_implemented
  let add n1 n2 = raise Not_implemented

#3 Keep signature_a0.mli and structure_a0.ml in the same folder
 A. Compile the signature file as
    ocamlc signature_a0.mli
    This creates a .cmi files
 B. Use the top level to test your code
    #use "structure_a0.ml";;
    open A0;;
    add (mk_big 5) (mk_big 10);;

#4 Grading criteria:
- Correctness
- Efficiency
- Code readability, comments etc

*)
