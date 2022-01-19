# Arithmetic for Arbitrarily Large Numbers

## Execution

- Keep signature_a0.mli and structure_a0.ml in the same folder

- Compile the signature file as
    
        ocamlc signature_a0.mli
    
  This creates a .cmi file

- Use the top level to execute functions
    
        #use "structure_a0.ml";;
        open A0;;
        add (mk_big 5) (mk_big 10);;

## Description

The [BigInt](signature_a0.mli) module defines the following functions/operators on bigints for arithmetic on arbitrarily large numbers:

```ocaml
(* Arithmetic *)
val add: bigint -> bigint -> bigint (* Addition *)
val mult: bigint -> bigint -> bigint (* Multiplication *)
val sub: bigint -> bigint -> bigint (* Subtraction *)
val div: bigint -> bigint -> bigint (* Quotient *)
val rem: bigint -> bigint -> bigint (* Remainder *)
val minus: bigint -> bigint (* Unary negation *)
val abs: bigint -> bigint (* Absolute value *)

(* Comparison operations:  *)
val eq: bigint -> bigint -> bool (* Equal *)
val gt:  bigint -> bigint -> bool (* Greater_than. *)
val lt:  bigint -> bigint -> bool (* Less_than. *)
val geq:  bigint -> bigint -> bool (* Great_or_equal. *)
val leq:  bigint -> bigint -> bool (* Less_or_equal.  *)
```

The following functions are defined for conversion between bigint to string and int to bigint:

```ocaml
val print_num: bigint -> string
val mk_big:  int -> bigint
```