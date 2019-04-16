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
  | Tup of (int * answer list)
  | CL of string * opcode list * environment

and environment = (string * answer) list

type opcode =
  | LOOKUP of string
  | CLOS of expr * opcode list
  | RET
  | APP
  | PLUS
  | TIMES
  | AND
  | OR
  | BOOL of bool
  | INT of int
  | CMP
  | IFTE
  (*  *)
  | ABS
  | UNARYMINUS
  | NOT
  | MINUS
  | DIV
  | REM
  | EQS
  | GTE
  | LTE
  | GT
  | LT
  | LPAREN
  | RPAREN
  | TUPLE of int
  | PROJ of int * int
  | LET
  | FABS
  | FCALL

exception Variable_not_found

exception Invalid_expression

exception Drop_number_exceeds_list

let rec compile_tuple x acc =
  match x with [] -> acc | hd :: tl -> compile_tuple tl acc @ compile hd

let rec compile e =
  match e with
  | V x -> [LOOKUP x]
  | Lambda (a, b) -> CLOS (a, compile b @ [RET]) (* WARNING: Verify this*)
  | App (a, b) -> compile a @ compile b @ [APP]
  | Plus (a, b) -> compile a @ compile b @ [PLUS]
  | Mult (a, b) -> compile a @ compile b @ [TIMES]
  | And (a, b) -> compile a @ compile b @ [AND]
  | Or (a, b) -> compile a @ compile b @ [OR]
  | Bool a -> [BOOL a]
  | Integer a -> [INT a]
  | Cmp a -> compile a @ [CMP]
  | If_Then_Else (a, b, c) -> compile c @ compile b @ compile a @ [IFTE]
  (*  *)
  | Equals (e1, e2) -> compile e1 @ compile e2 @ [EQS]
  | GreaterTE (e1, e2) -> compile e1 @ compile e2 @ [GTE]
  | LessTE (e1, e2) -> compile e1 @ compile e2 @ [LTE]
  | GreaterT (e1, e2) -> compile e1 @ compile e2 @ [GT]
  | LessT (e1, e2) -> compile e1 @ compile e2 @ [LT]
  | InParen e1 -> [PAREN] @ compile e1 @ [PAREN]
  | Sub (e1, e2) -> compile e1 @ compile e2 @ [MINUS]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [DIV]
  | Rem (e1, e2) -> compile e1 @ compile e2 @ [REM]
  | Abs e1 -> compile e1 @ [ABS]
  | Negative e1 -> compile e1 @ [UNARYMINUS]
  | Not e1 -> compile e1 @ [NOT]
  | Tuple (e1, e2) ->
      if List.length e2 = e1 then compile_tuple e2 [] @ [TUPLE e1]
      else raise Illegal_Tuple
  | Project ((e1, e2), e3) -> compile e3 @ [PROJ (e1, e2)]

let rec drop l n =
  match l with
  | [] -> if n = 0 then [] else raise Drop_number_exceeds_list
  | hd :: tl -> if n = 0 then l else drop tl (n - 1)

let get_int (a : answer) = match a with Integer e1 -> e1

let get_bool (a : answer) = match a with Bool e1 -> e1

let rec find_variable str env : answer =
  match env with
  | (name, cl) :: tl -> if str = name then cl else find_variable str tl
  | [] -> raise Variable_not_found

let rec tuple_stack_calc stack n acc =
  match n with
  | 0 -> acc
  | a -> tuple_stack_calc (List.tl stack) (a - 1) (acc @ [List.hd stack])

let rec proj_stack_calc tupl n =
  match tupl with
  | Tup (a, b) -> List.hd (drop b (n - 1))
  | _ -> raise Illegal_Tuple

let rec find_paren list accumulator =
  match list with
  | RPAREN :: e -> accumulator
  | a :: b -> find_paren b accumulator @ [a]
  | [] -> raise Ill_Formed_Stack

let rec secd (stck : answer list) (env : environment) (op_list : opcode list)
    (dump : closure list * environment * opcode list) : value_closure =
  match op_list with
  | CLOS (a, l) :: cmd -> (
    match a with
    | V n -> secd (CL (n, l, env) :: stck) env cmd dump
    | _ -> raise Invalid_expression )
  | APP :: cmd -> (
      let stck_tail = List.tl (List.tl stck) param = List.hd stck in
      let clos = List.hd (List.tl stck) in
      let param = List.hd stck in
      match clos with
      | CL (a, b, c) ->
          secd [] ((a, param) :: c) b ((stck_tail, env, cmd) :: dump)
      | _ -> raise Invalid_expression )
  | RET :: cmd -> (
      let func_ans = List.hd stck in
      let dump_head = List.hd dump in
      match dump_head with
      | p1, p2, p3 -> secd func_ans :: p1 p2 p3 (List.tl dump)
      | _ -> raise Invalid_expression )
  | LOOKUP x :: cmd -> secd (find_variable x env) :: stck env cmd dump
  | INT x :: cmd -> secd (Integer x :: stck) env cmd dump
  | BOOL x :: cmd -> secd (Bool x :: stck) env cmd dump
  | PLUS :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Integer (op1 + op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | TIMES :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Integer (op1 * op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | AND :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Bool op1, Bool op2 ->
          secd (Bool (op1 && op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | OR :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Bool op1, Bool op2 ->
          secd (Bool (op1 || op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | CMP :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | Integer op1 -> secd (Bool (op1 > 0) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | IFTE :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let v3 = List.hd (List.tl (List.tl stck)) in
      let stck_tail = List.tl (List.tl (List.tl stck)) in
      match (v1, v2, v3) with
      | Bool op1, op2, op3 ->
          if op1 then secd (op2 :: stck_tail) env cmd dump
          else secd (op3 :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  (*  *)
  | MINUS :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Integer (op1 - op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | DIV :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Integer (op1 / op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | REM :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Integer (op1 mod op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | ABS :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | Integer op1 ->
          if op1 >= 0 then secd (Integer op1 :: stck_tail) env cmd dump
          else secd (Integer (-op1) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | UNARYMINUS :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | Integer op1 -> secd (Integer (-op1) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | EQS :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Bool (op1 = op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | GTE :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Bool (op1 >= op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | LTE :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Bool (op1 <= op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | GT :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Bool (op1 > op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | LT :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | Integer op1, Integer op2 ->
          secd (Bool (op1 < op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | LPAREN :: cmd ->
      let paren_op = find_paren e [] in
      let temp = secd [] env paren_op [] in
      secd (temp :: stck) env (drop cmd (List.length paren_op) + 1) dump
  | NOT :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | Bool op1 -> secd (Bool (not op1) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | TUPLE e1 :: cmd ->
      secd (Tup (e1, tuple_stack_calc stck e1 []) :: drop stck e1) env cmd dump
  | PROJ (e1, e2) :: cmd ->
      secd (proj_stack_calc (List.hd stck) e1 :: drop stck 1) env cmd dump
  | [] -> List.hd stck

let secd_machine e rho = secd [] rho (compile e) []

(* make sure secd_mc doesn't look below stack line *)
