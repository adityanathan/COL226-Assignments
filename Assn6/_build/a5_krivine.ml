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
  | Let of definition * expr

(* definition *)
and definition =
  | Simple of string * expr * exptype
  | Sequence of definition list
  | Parallel of definition list
  | Local of definition * definition

and answer =
  | IntVal of int
  | BoolVal of bool
  | CL of expr * environment
  | Tup of int * answer list

and environment = (string * answer) list

and exptype =
  | Tint
  | Tunit
  | Tbool
  | Ttuple of exptype list
  | Tfunc of (exptype * exptype)

exception Illegal_Tuple

exception Variable_not_found

exception Invalid_expression

exception Projection_Index_Out_of_bounds

let rec find_variable str env : answer =
  match env with
  | (name, cl) :: tl -> if str = name then cl else find_variable str tl
  | [] -> raise Variable_not_found

let rec eval_tuple acc list rho =
  match list with
  | [] -> acc
  | hd :: tl -> eval_tuple (acc @ [krv_mc (CL (hd, rho)) []]) tl rho

and eval_projection a b l rho =
  if b = List.length l && a <= b && a >= 1 then
    match l with
    | [] -> raise Projection_Index_Out_of_bounds
    | hd :: tl -> if a = 1 then hd else eval_projection (a - 1) (b - 1) tl rho
  else if a < 1 || a > b then raise Projection_Index_Out_of_bounds
  else raise Illegal_Tuple

and krv_mc (cl : answer) (stck : answer list) =
  match cl with
  | CL (V x, env) -> krv_mc (find_variable x env) stck
  | CL (Lambda (param, f_body, func_type), env) -> (
    match param with
    | (x : string) ->
        krv_mc (CL (f_body, (param, List.hd stck) :: env)) (List.tl stck)
    | _ -> raise Invalid_expression )
  | CL (App (func, param), env) ->
      krv_mc (CL (func, env)) (CL (param, env) :: stck)
  | CL (Plus (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> IntVal (v1 + v2) )
  | CL (Mult (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> IntVal (v1 * v2) )
  | CL (And (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with BoolVal v1, BoolVal v2 -> BoolVal (v1 && v2) )
  | CL (Or (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with BoolVal v1, BoolVal v2 -> BoolVal (v1 || v2) )
  | CL (Bool a, env) -> BoolVal a
  | CL (Integer a, env) -> IntVal a
  | CL (Cmp a, env) -> (
    match krv_mc (CL (a, env)) stck with IntVal v1 ->
      if v1 > 0 then BoolVal true else BoolVal false )
  | CL (If_Then_Else (cond, e1, e2), env) -> (
    match krv_mc (CL (cond, env)) stck with BoolVal v1 ->
      if v1 = true then krv_mc (CL (e1, env)) stck
      else krv_mc (CL (e2, env)) stck )
  (*  *)
  | CL (Equals (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> BoolVal (v1 = v2) )
  | CL (GreaterTE (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> BoolVal (v1 >= v2) )
  | CL (LessTE (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> BoolVal (v1 <= v2) )
  | CL (GreaterT (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> BoolVal (v1 > v2) )
  | CL (LessT (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> BoolVal (v1 < v2) )
  | CL (InParen a, env) -> krv_mc (CL (a, env)) []
  | CL (Tuple (e1, e2), env) ->
      if e1 = List.length e2 then Tup (e1, eval_tuple [] e2 env)
      else raise Illegal_Tuple
  | CL (Project ((e1, e2), e3), env) -> (
    match krv_mc (CL (e3, env)) [] with
    | Tup (a, b) ->
        if a = e2 then eval_projection e1 e2 b env else raise Illegal_Tuple
    | _ -> raise Illegal_Tuple )
  | CL (Sub (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> IntVal (v1 - v2) )
  | CL (Div (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> IntVal (v1 / v2) )
  | CL (Rem (a, b), env) -> (
    match (krv_mc (CL (a, env)) stck, krv_mc (CL (b, env)) stck)
    with IntVal v1, IntVal v2 -> IntVal (v1 mod v2) )
  | CL (Negative a, env) -> (
    match krv_mc (CL (a, env)) stck with IntVal v1 -> IntVal (-v1) )
  | CL (Not a, env) -> (
    match krv_mc (CL (a, env)) stck with BoolVal v1 -> BoolVal (not v1) )
  | CL (Abs a, env) -> (
    match krv_mc (CL (a, env)) stck with IntVal v1 ->
      if v1 > 0 then IntVal v1 else IntVal (-v1) )

let rec make_closure_list binding =
  match binding with
  | (str, b) :: tl -> (str, CL (b, [])) :: make_closure_list tl
  | [] -> []

let krivine_machine e rho = krv_mc (CL (e, make_closure_list rho)) []

(* accomodate change of closure to type answer *)
