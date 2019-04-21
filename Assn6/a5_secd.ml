open A5_type;;
open A4;;

exception Not_implemented

type answer =
  | IntVal of int
  | BoolVal of bool
  | Tup of (int * answer list)
  | CL of string * opcode list * environment

and environment = (string * answer) list

and opcode =
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

exception Illegal_Tuple

let rec compile_tuple x acc =
  match x with [] -> acc | hd :: tl -> compile_tuple tl acc @ compile hd

and compile e =
  match e with
  | V x -> [LOOKUP x]
  | Lambda (a, b, c) -> [CLOS (V a, compile b @ [RET])]
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
	| RecursiveLambda (f_name, a, b, c, d) -> raise Not_implemented
  | Equals (e1, e2) -> compile e1 @ compile e2 @ [EQS]
  | GreaterTE (e1, e2) -> compile e1 @ compile e2 @ [GTE]
  | LessTE (e1, e2) -> compile e1 @ compile e2 @ [LTE]
  | GreaterT (e1, e2) -> compile e1 @ compile e2 @ [GT]
  | LessT (e1, e2) -> compile e1 @ compile e2 @ [LT]
  | InParen e1 -> [LPAREN] @ compile e1 @ [RPAREN]
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

let get_int (a : answer) = match a with IntVal e1 -> e1

let get_bool (a : answer) = match a with BoolVal e1 -> e1

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
  | a :: b -> find_paren b (accumulator @ [a])
  | [] -> raise Invalid_expression

let rec secd (stck : answer list) (env : environment) (op_list : opcode list) (dump : ((answer list) * environment * (opcode list)) list) : answer =
  match op_list with
  | CLOS (a, l) :: cmd -> (
    match a with
    | V n -> secd (CL (n, l, env) :: stck) env cmd dump
    | _ -> raise Invalid_expression )
  | APP :: cmd -> (
      let stck_tail = List.tl (List.tl stck) in
      let clos = List.hd (List.tl stck) in
      let param = List.hd stck in
			match clos with
      | CL (a, b, c) ->
          secd [] ((a, param) :: c) b ((stck_tail, env, cmd) :: dump)
      | _ -> raise Invalid_expression )
	(* | RECURSIVE_CLOS(f_name, a, l) :: cmd -> (
		match a with
		| V n -> secd (CL (n, l, env)::stck) ((f_name, CL(n, l, env))::env) cmd dump
		| _ -> raise Invalid_expression
		(* Here I am binding the function name to the body in the global space.
		So in this case a later function call outside the function body will also work.*)
		) *)
  | RET :: cmd -> (
      let func_ans = List.hd stck in
      let dump_head = List.hd dump in
      match dump_head with
      | p1, p2, p3 -> secd (func_ans :: p1) p2 p3 (List.tl dump)
      | _ -> raise Invalid_expression )
  | LOOKUP x :: cmd -> secd ((find_variable x env) :: stck) env cmd dump
  | INT x :: cmd -> secd (IntVal x :: stck) env cmd dump
  | BOOL x :: cmd -> secd (BoolVal x :: stck) env cmd dump
  | PLUS :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (IntVal (op1 + op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | TIMES :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (IntVal (op1 * op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | AND :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | BoolVal op1, BoolVal op2 ->
          secd (BoolVal (op1 && op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | OR :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | BoolVal op1, BoolVal op2 ->
          secd (BoolVal (op1 || op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | CMP :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | IntVal op1 -> secd (BoolVal (op1 > 0) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | IFTE :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let v3 = List.hd (List.tl (List.tl stck)) in
      let stck_tail = List.tl (List.tl (List.tl stck)) in
      match (v1, v2, v3) with
      | (BoolVal op1), op2, op3 ->
          if op1 then secd (op2 :: stck_tail) env cmd dump
          else secd (op3 :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  (*  *)
  | MINUS :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (IntVal (op1 - op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | DIV :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (IntVal (op1 / op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | REM :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (IntVal (op1 mod op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | ABS :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | IntVal op1 ->
          if op1 >= 0 then secd (IntVal op1 :: stck_tail) env cmd dump
          else secd (IntVal (-op1) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | UNARYMINUS :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | IntVal op1 -> secd (IntVal (-op1) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | EQS :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (BoolVal (op1 = op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | GTE :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (BoolVal (op1 >= op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | LTE :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (BoolVal (op1 <= op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | GT :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (BoolVal (op1 > op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | LT :: cmd -> (
      let v1 = List.hd stck in
      let v2 = List.hd (List.tl stck) in
      let stck_tail = List.tl (List.tl stck) in
      match (v1, v2) with
      | IntVal op1, IntVal op2 ->
          secd (BoolVal (op1 < op2) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | LPAREN :: cmd ->
      let paren_op = find_paren cmd [] in
      let temp = secd [] env paren_op [] in
      secd (temp :: stck) env (drop cmd ((List.length paren_op) + 1)) dump
  | NOT :: cmd -> (
      let v1 = List.hd stck in
      let stck_tail = List.tl stck in
      match v1 with
      | BoolVal op1 -> secd (BoolVal (not op1) :: stck_tail) env cmd dump
      | _ -> raise Invalid_expression )
  | TUPLE e1 :: cmd ->
      secd (Tup (e1, tuple_stack_calc stck e1 []) :: drop stck e1) env cmd dump
  | PROJ (e1, e2) :: cmd ->
      secd (proj_stack_calc (List.hd stck) e1 :: drop stck 1) env cmd dump
  | [] -> List.hd stck
	| _ -> raise Invalid_expression

exception Type_error
let secd_machine e rho type_rho= try
let dummy = type_infer type_rho e in
secd [] rho (compile e) []
with
Type_infer_invalid -> raise Type_error


(* make sure secd_mc doesn't look below stack line *)
