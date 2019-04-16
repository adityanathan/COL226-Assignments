type expr =
  | V of string
  | Lambda of (expr * expr)
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
  | Abs of exp
  | Negative of exp
  | Not of exp
  | Sub of exp * exp
  | Div of exp * exp
  | Rem of exp * exp
  | Equals of exp * exp
  | GreaterTE of exp * exp
  | LessTE of exp * exp
  | GreaterT of exp * exp
  | LessT of exp * exp
  | InParen of exp
  | Tuple of int * exp list
  | Project of (int * int) * exp

type answer =
  | Integer of int
  | Bool of bool
  | CL of expr * environment
  | Tup of int * answer list

and environment = (string * answer) list

exception Illegal_Tuple

exception Variable_not_found

exception Invalid_expression

let rec find_variable str env : closure =
  match env with
  | (name, cl) :: tl -> if str = name then cl else find_variable str tl
  | [] -> raise Variable_not_found

let rec eval_tuple acc list rho =
  match list with
  | [] -> acc
  | hd :: tl -> eval_tuple (acc @ [krv_mc CL (hd, rho) []]) tl rho

and eval_projection a b l rho =
  if b = List.length l && a <= b && a >= 1 then
    match l with
    | [] -> raise Projection_Index_Out_of_bounds
    | hd :: tl -> if a = 1 then hd else eval_projection (a - 1) (b - 1) tl rho
  else if a < 1 || a > b then raise Projection_Index_Out_of_bounds
  else raise Illegal_Tuple

and krv_mc (cl : closure) (stck : closure list) =
  match cl with
  | CL (V x, env) -> krv_mc (find_variable x env) stck
  | CL (Lambda (param, f_body), env) -> (
    match param with
    | V x -> krv_mc CL (f_body, (param, List.hd stck) :: env) (List.tl stck)
    | _ -> raise Invalid_expression )
  | CL (App (func, param), env) ->
      krv_mc CL (func, env) (CL (param, env) :: stck)
  | CL (Plus (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Integer v1 + v2, env)
    )
  | CL (Mult (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Integer v1 * v2, env)
    )
  | CL (And (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Bool v1, e1), CL (Bool v2, e2) -> CL (Bool v1 && v2, env) )
  | CL (Or (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Bool v1, e1), CL (Bool v2, e2) -> CL (Bool v1 || v2, env) )
  | CL (Bool a, env) -> CL (Bool a, env)
  | CL (Int a, env) -> CL (Int a, env)
  | CL (Cmp a, env) -> (
    match krv_mc CL (a, env) stck with CL (Integer v1, e1) ->
      if v1 > 0 then CL (Bool true, env) else CL (Bool false, env) )
  | CL (If_Then_Else (cond, e1, e2), env) -> (
    match krv_mc CL (cond, env) stck with CL (Bool v1, env1) ->
      if v1 = true then krv_mc CL (e1, env) stck else krv_mc CL (e2, env) stck
    )
  (*  *)
  | CL (Equals (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Bool v1 = v2, env) )
  | CL (GreaterTE (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Bool v1 >= v2, env) )
  | CL (LessTE (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Bool v1 <= v2, env) )
  | CL (GreaterT (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Bool v1 > v2, env) )
  | CL (LessT (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Bool v1 < v2, env) )
  | CL (InParen a, env) -> krv_mc CL (a, env) []
  | CL (Tuple (a, b), env) ->
      if e1 = List.length e2 then CL (Tup (e1, eval_tuple [] e2 rho), env)
      else raise Illegal_Tuple
  | CL (Project ((e1, e2), e3), env) -> (
    match krv_mc CL (e3, env) [] with
    | Tup (a, b) ->
        if a = e2 then eval_projection e1 e2 b env else raise Illegal_Tuple
    | _ -> raise Illegal_Tuple )
  | CL (Sub (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Integer v1 - v2, env)
    )
  | CL (Div (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Integer v1 / v2, env)
    )
  | CL (Rem (a, b), env) -> (
    match (krv_mc CL (a, env) stck, krv_mc CL (b, env) stck)
    with CL (Integer v1, e1), CL (Integer v2, e2) -> CL (Integer v1 mod v2, env)
    )
  | CL (Negative a, env) -> (
    match krv_mc CL (a, env) stck with CL (Integer v1, e1) ->
      CL (Integer - v2, env) )
  | CL (Not a, env) -> (
    match krv_mc CL (a, env) stck with CL (Bool v1, e1) ->
      CL (Bool (not v2), env) )
  | CL (Abs a, env) -> (
    match krv_mc CL (a, env) stck with CL (Integer v1, e1) ->
      if v1 > 0 then CL (Integer v2, env) else CL (Integer - v2, env) )

let make_closure_list binding =
  match binding with
  | (str, b) :: tl -> (str, CL (b, [])) :: make_closure_list tl
  | [] -> []

let krivine_machine e rho = krv_mc CL (e, make_closure_list rho) []

(* accomodate change of closure to type answer *)
