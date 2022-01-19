# Type Checker

## Language Extension

The language is extended to support:

- Definitions
    
    + Simple: `def x = expr`
    + Sequential: `d1; d2`
    + Parallel: `d1 || d2`
    + Local Scoping: `local d1 in d2 end`

- Functions

    + Function abstractions with one input argument - `\x. expr`
    + Function Call - `e1(e2)`

- Tuples

    + n-tuple: `(a, b, ....)`
    + Projection (Get the ith element of an n-tuple): `Proj((i,n), e)` where 0 <= i <= n

## Type Checking

Types for the language are defined as:

```ocaml
type exptype = 
    | Tint 
    | Tunit 
    | Tbool 
    | Ttuple of (exptype list)     (* Tuple type *)
    | Tfunc of (exptype * exptype) (* Function Abstraction Type *)
```

As for the *type-checker* for the language, we define two functions `hastype` and `yields`,

- `hastype` represents the association `G |- e : t`. The function `hastype` takes a set of type assumptions `G` (represented as a list of tuples of the form (variable name, type)), an expression `e`, and an expression type `t`, and decides if the expression `e` has the claimed type `t` under the given type assumptions `G`.

```ocaml
val hastype : type_assumptions -> expression -> exptype -> bool
```

- `yields` represents the association `G |- d : G'`. The function `yields` takes a set of type assumptions `G`, a definition `d`, and another set of type assumptions `G'` and decides whether under the given assumptions `G`, the definition `d` yields the type assumptions `G'` or not.

```ocaml
val yields: type_assumptions -> definition -> type_assumptions -> bool
```

