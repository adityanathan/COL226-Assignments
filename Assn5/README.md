# Krivine and SECD Machine

This module implements two interpreters for the functional language defined so far,

- Krivine Machine *(in closure form)* that implements **Call by Name** semantics for function calls

    + Given an expression `e`, environment `E` to evaluate `e` in, and the type for each binding in `E` as a list of (variable name, type), the machine first checks that the bindings in `E` are properly typed. Then, it packages `e` and its environment `E` into a closure and evaluates the closure starting with an empty stack (see the `krv_mc` function in [a5_krivine.ml](a5_krivine.ml))

```ocaml
val krv_mc: closure -> stack -> answer (* Helper for krivine_machine *)
val krivine_machine: expr -> environment -> environment bindings' types -> answer
```

- SECD Machine that implements **Call by Value** semantics for function calls

    + Given an expression `e`, environment `E` to evaluate `e` in, and the type for each binding in `E` as a list of (variable name, type), the machine first checks that the bindings in `E` are properly typed. Then, it compiles the expression `e` into a compiled list of opcodes in Reverse Polish Notation and evaluates this compiled list using `E` and starting with an empty stack and empty dump (see the `secd` function in [a5_secd.ml](a5_secd.ml))

```ocaml
val compile: expr -> opcode list
val secd: stack -> environment -> opcode list -> dump -> answer (* Helper for secd_machine *)
val secd_machine: expr -> environment -> environment bindings' types -> answer
```