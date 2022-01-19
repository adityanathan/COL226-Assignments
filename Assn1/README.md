# Definitional Interpreter and Stack Machine

## Execution

- Run the following command from the terminal

        ocamlbuild a1.cmo a0.cmo

  This will create a _build directory and then the functions can be used from ocaml top level launched with this directory as current working directory

## Description

The interface for this module described below is defined in [a1.mli](a1.mli)

The abstract syntax of the calculator language is characterized by the type `exptree`,

```ocaml
type exptree =
  | N of int
  | Plus of exptree * exptree
  | Minus of exptree * exptree
  | Mult of exptree * exptree
  | Div of exptree * exptree
  | Rem of exptree * exptree
  | Neg of exptree
  | Abs of exptree
```

Semantic meaning for this calculator language is given by the following definitional interpreter defined as a function `eval`,

```ocaml
val eval : exptree -> int
```

The opcodes for the stack-based machine are given by the type `opcode`,

```ocaml
type opcode =
  | CONST of bigint
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | REM
  | ABS
  | UNARYMINUS
```

The stack-based machine that accepts these opcodes in Reverse Polish Notation (RPN) is defined as a tail-recursive function `stackmc`,

```ocaml
val stackmc : bigint list -> opcode list -> bigint
```

The compiler that converts a given expression's AST to opcodes in RPN that the stack-based machine can accept is defined by `compile`,

```ocaml
val compile : exptree -> opcode list
```

It is also proven that for all inputs

```ocaml
eval t = stackmc lst (compile t)
```