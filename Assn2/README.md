# Lexer

A lexer has been built as defined in [scanner.mll](scanner.mll) using ocamllex. The language is designed to support operations on integers and booleans, if-then-else and variable bindings.

## Execution

- Execute the following to compile the lexer defined in scanner.mll to Ocaml

        ocamllex scanner.mll

- Open a REPL in this directory

- To lex a string, use

        scanner "<string to be lexed>"

  For example,

        scanner "T ^ T"

