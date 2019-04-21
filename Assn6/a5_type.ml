type expr =
  | V of string
  | Lambda of (string * expr * exptype)
  | App of (expr * expr)
  | Plus of (expr * expr)
  | Mult of (expr * expr)
  | And of (expr * expr)
  | Or of (expr * expr)
  | Bool of bool
  | Integer of int
  | Cmp of expr
  | If_Then_Else of (expr * expr * expr)
  (*  *)
  | Abs of expr
  | Negative of expr
  | Not of expr
  | Sub of expr * expr
  | Div of expr * expr
  | Rem of expr * expr
  | Equals of expr * expr
  | GreaterTE of expr * expr
  | LessTE of expr * expr
  | GreaterT of expr * expr
  | LessT of expr * expr
  | InParen of expr
  | Tuple of int * expr list
  | Project of (int * int) * expr
	| RecursiveLambda of string * string * expr * exptype * exptype

and exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)
