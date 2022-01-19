# COL226-Assignments

A tiny functional language built as part of the COL226 Programming Languages course taught by [Prof. Sanjiva Prasad](https://www.cse.iitd.ernet.in/~sanjiva/) at IIT Delhi in 2019

- [Assignment 0](Assn0/README.md) - Arithmetic for arbitrarily large numbers
	
    - Defines a BIGINT package in OCaml for implementing arithmetic for arbitrarily large numbers, using lists of digits to represent an arbitrarily large number.

- [Assignment 1](Assn1/README.md) - A simple definitional interpreter and a stack machine
	
    - Models the "abstract syntax" of a simple calculator language for BIGINT expressions, and gives it "semantic meaning" in terms of OCaml's built-in types.
	
    - Implements a simple stack-based machine which accepts opcodes in Reverse Polish Notation (RPN) as input and evaluates them.
	
    - Implements a compiler to translate abstract syntax of an expression in the calculator language to opcodes in RPN that the stack-based machine can accept by doing post-order traversal of the expression's AST.

- [Assignment 2](Assn2/README.md) - Building a lexer using ocamllex
    
    - Extends the language with boolean values, if-then-else and variable bindings and implements a lexer for the language.

- [Assignment 3](Assn3/a3.mly) - Building a parser using ocamlyacc

    - Implements a grammar for the language using ocamlyacc.

- [Assignment 4](Assn4/README.md) - Type Checker

    - Extends the language with tuples, definitions and function abstractions and implements a type checker for this language.

- [Assignment 5](Assn5/README.md) - Interpreters: Krivine and SECD Machine

    - Implements the Krivine Machine (Call by Name Interpreter) and SECD Machine (Call by Value Interpreter) for the defined functional language.

- [Assignment 6](Assn6) - A simulator for nested procedure calls

    - Implements a simulator for the call stack during nested procedure calls in order to understand implementations of static scoping in Algol-like languages, particularly visibility rules and the set-up/tear-down involved in a procedure call and return. Note: The actual call stack layout for a compiler and code generation are not implemented.