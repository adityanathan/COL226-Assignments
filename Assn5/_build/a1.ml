open A0

exception Invalid_Expression

exception Empty_input

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

(* abstract syntax  *)
type exptree =
  | Var of string
  (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int
  (* Integer constant *)
  | B of bool
  (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree
  (* abs *)
  | Negative of exptree
  (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree
  (* Addition + *)
  | Sub of exptree * exptree
  (* Subtraction - *)
  | Mult of exptree * exptree
  (* Multiplication * *)
  | Div of exptree * exptree
  (* div *)
  | Rem of exptree * exptree
  (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree
  (* conjunction /\ *)
  | Disjunction of exptree * exptree
  (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree
  (* = *)
  | GreaterTE of exptree * exptree
  (* >= *)
  | LessTE of exptree * exptree
  (* <= *)
  | GreaterT of exptree * exptree
  (* > *)
  | LessT of exptree * exptree
  (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree
  (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree
  (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * exptree list
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int * int) * exptree
  | Let of definition * exptree
  | FunctionAbstraction of string * exptree * exptype
  | FunctionCall of exptree * exptree

(* definition *)
and definition =
  | Simple of string * exptree * exptype
  | Sequence of definition list
  | Parallel of definition list
  | Local of definition * definition

(* unary operators on integers *)

type opcode =
  | VAR of string
  | NCONST of bigint
  | BCONST of bool
  | ABS
  | UNARYMINUS
  | NOT
  | PLUS
  | MINUS
  | MULT
  | DIV
  | REM
  | CONJ
  | DISJ
  | EQS
  | GTE
  | LTE
  | GT
  | LT
  | PAREN
  | IFTE
  | TUPLE of int
  | PROJ of int * int
  | LET
  | FABS
  | FCALL
  | SIMPLEDEF
  | SEQCOMPOSE
  | PARCOMPOSE
  | LOCALDEF

exception Zero_exp_zero_error

let rec eval_b_exponent acc x y =
  match y with
  | NonNeg, [] ->
      if x = (NonNeg, []) then raise Zero_exp_zero_error else (NonNeg, [1])
  | NonNeg, [1] -> mult acc x
  | power -> eval_b_exponent (mult acc x) x (sub y (NonNeg, [1]))

let rec eval_exponent acc x y =
  match y with
  | 0 -> if x = 0 then raise Zero_exp_zero_error else 1
  | 1 -> acc * x
  | power -> eval_exponent (acc * x) x (y - 1)

exception Illegal_Tuple

exception Projection_Index_Out_of_bounds

let rec eval_tuple acc list rho =
  match list with
  | [] -> acc
  | hd :: tl -> eval_tuple (acc @ [eval hd rho]) tl rho

and eval_projection a b l rho =
  if b = List.length l && a <= b && a >= 1 then
    match l with
    | [] -> raise Projection_Index_Out_of_bounds
    | hd :: tl -> if a = 1 then hd else eval_projection (a - 1) (b - 1) tl rho
  else if a < 1 || a > b then raise Projection_Index_Out_of_bounds
  else raise Illegal_Tuple

and eval exp rho =
  match exp with
  (* Done -> raise Empty_input *)
  | N number -> NumVal number
  | B value -> BoolVal value
  | Var var_name -> rho var_name
  | Conjunction (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | BoolVal a, BoolVal b -> BoolVal (a && b)
    | _, _ -> raise Invalid_Expression )
  | Disjunction (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | BoolVal a, BoolVal b -> BoolVal (a || b)
    | _, _ -> raise Invalid_Expression )
  | Equals (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> BoolVal (a = b)
    | _, _ -> raise Invalid_Expression )
  | GreaterTE (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> BoolVal (a >= b)
    | _, _ -> raise Invalid_Expression )
  | LessTE (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> BoolVal (a <= b)
    | _, _ -> raise Invalid_Expression )
  | GreaterT (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> BoolVal (a > b)
    | _, _ -> raise Invalid_Expression )
  | LessT (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> BoolVal (a < b)
    | _, _ -> raise Invalid_Expression )
  | InParen e1 -> eval e1 rho
  | IfThenElse (e1, e2, e3) -> (
    match eval e1 rho with
    | BoolVal a -> if a = true then eval e2 rho else eval e3 rho
    | _ -> raise Invalid_Expression )
  | Tuple (e1, e2) ->
      if e1 = List.length e2 then TupVal (e1, eval_tuple [] e2 rho)
      else raise Illegal_Tuple
  | Project ((e1, e2), e3) -> (
    match eval e3 rho with
    | TupVal (a, b) ->
        if a = e2 then eval_projection e1 e2 b rho else raise Illegal_Tuple
    | _ -> raise Illegal_Tuple )
  | Add (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> NumVal (a + b)
    | _, _ -> raise Invalid_Expression )
  | Sub (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> NumVal (a - b)
    | _, _ -> raise Invalid_Expression )
  | Mult (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> NumVal (a * b)
    | _, _ -> raise Invalid_Expression )
  | Div (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b ->
        if a < 0 && b > 0 && b * (a / b) <> a then NumVal ((a / b) - 1)
        else NumVal (a / b)
    (* Done to make sure Euclidean Division is satisfied *)
    (* To make remainder always greater than 0 *)
    | _, _ -> raise Invalid_Expression )
  | Rem (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b ->
        if a < 0 && b > 0 && b * (a / b) <> a then NumVal ((a mod b) + b)
        else NumVal (a mod b)
    (* Done to make sure Euclidean Division is satisfied *)
    (* To make remainder always greater than 0 *)
    | _, _ -> raise Invalid_Expression )
  (* | Exp (e1, e2) -> (
    match (eval e1 rho, eval e2 rho) with
    | NumVal a, NumVal b -> NumVal (eval_exponent 1 a b)
    | _, _ -> raise Invalid_Expression ) *)
  | Negative e1 -> (
    match eval e1 rho with
    | NumVal a -> NumVal (-1 * a)
    | _ -> raise Invalid_Expression )
  | Not e1 -> (
    match eval e1 rho with
    | BoolVal a -> BoolVal (not a)
    | _ -> raise Invalid_Expression )
  | Abs e1 -> (
    match eval e1 rho with
    | NumVal a -> if a < 0 then NumVal (-1 * a) else NumVal a
    | _ -> raise Invalid_Expression )

(* | _ -> raise Invalid_Expression *)

let rec compile_tuple x acc =
  match x with [] -> acc | hd :: tl -> compile_tuple tl acc @ compile hd

and compile exp =
  match exp with
  (* Done -> [DONE] *)
  | Var x -> [VAR x]
  | N number -> [NCONST (mk_big number)]
  | B value -> [BCONST value]
  | Equals (e1, e2) -> compile e1 @ compile e2 @ [EQS]
  | GreaterTE (e1, e2) -> compile e1 @ compile e2 @ [GTE]
  | LessTE (e1, e2) -> compile e1 @ compile e2 @ [LTE]
  | GreaterT (e1, e2) -> compile e1 @ compile e2 @ [GT]
  | LessT (e1, e2) -> compile e1 @ compile e2 @ [LT]
  | Conjunction (e1, e2) -> compile e1 @ compile e2 @ [CONJ]
  | Disjunction (e1, e2) -> compile e1 @ compile e2 @ [DISJ]
  | InParen e1 -> [PAREN] @ compile e1 @ [PAREN]
  | Add (e1, e2) -> compile e1 @ compile e2 @ [PLUS]
  | Sub (e1, e2) -> compile e1 @ compile e2 @ [MINUS]
  | Mult (e1, e2) -> compile e1 @ compile e2 @ [MULT]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [DIV]
  | Rem (e1, e2) -> compile e1 @ compile e2 @ [REM]
  | Abs e1 -> compile e1 @ [ABS]
  | Negative e1 -> compile e1 @ [UNARYMINUS]
  | Not e1 -> compile e1 @ [NOT]
  (* | Exp (e1, e2) -> compile e1 @ compile e2 @ [EXP] *)
  | IfThenElse (e1, e2, e3) -> compile e3 @ compile e2 @ compile e1 @ [IFTE]
  | Tuple (e1, e2) ->
      if List.length e2 = e1 then compile_tuple e2 [] @ [TUPLE e1]
      else raise Illegal_Tuple
  | Project ((e1, e2), e3) -> (
    match e3 with
    | Tuple (a, b) ->
        if e2 = a && e1 <= e2 && e1 >= 1 then compile e3 @ [PROJ (e1, e2)]
        else if e1 > e2 || e1 < 1 then raise Projection_Index_Out_of_bounds
        else raise Illegal_Tuple
    | Var x -> [VAR x] @ [PROJ (e1, e2)]
    | InParen x -> [PAREN] @ compile x @ [PAREN] @ [PROJ (e1, e2)]
    | _ -> raise Illegal_Tuple )

(* | _ -> raise Invalid_Expression *)

exception Drop_number_exceeds_list

exception Ill_Formed_Stack

exception Invalid_type

let rec drop l n =
  match l with
  | [] -> if n = 0 then [] else raise Drop_number_exceeds_list
  | hd :: tl -> if n = 0 then l else drop tl (n - 1)

(* type answer = BInt of bigint | Bool of bool *)

let get_int (a : answer) =
  match a with Num e1 -> e1 | _ -> raise Invalid_type

let get_bool (a : answer) =
  match a with Bool e1 -> e1 | _ -> raise Invalid_type

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
  | PAREN :: e -> accumulator
  | a :: b -> find_paren b accumulator @ [a]
  | [] -> raise Ill_Formed_Stack

let rec stackmc_prototype (acc : answer list) (op : opcode list) (a : int) rho
    =
  (* try *)
  match op with
  (* DONE :: e -> raise Empty_input *)
  | VAR (x : string) :: e -> stackmc_prototype (rho x :: acc) e a rho
  | NCONST (num : bigint) :: e -> stackmc_prototype (Num num :: acc) e a rho
  | BCONST (value : bool) :: e -> stackmc_prototype (Bool value :: acc) e a rho
  | PLUS :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Num (add (get_int (List.hd acc)) (get_int (List.hd (List.tl acc))))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | MULT :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Num (mult (get_int (List.hd acc)) (get_int (List.hd (List.tl acc))))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | MINUS :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Num (sub (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  (* | EXP :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Num
              (eval_b_exponent
                 (NonNeg, [1])
                 (get_int (List.hd (List.tl acc)))
                 (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack *)
  | DIV :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Num (div (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | REM :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Num (rem (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | ABS :: e ->
      if List.length acc >= a + 1 then
        stackmc_prototype
          (Num (abs (get_int (List.hd acc))) :: drop acc 1)
          e a rho
      else raise Ill_Formed_Stack
  | UNARYMINUS :: e ->
      if List.length acc >= a + 1 then
        stackmc_prototype
          (Num (minus (get_int (List.hd acc))) :: drop acc 1)
          e a rho
      else raise Ill_Formed_Stack
  | EQS :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (eq (get_int (List.hd acc)) (get_int (List.hd (List.tl acc))))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | GTE :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (geq (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | LTE :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (leq (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | GT :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (gt (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | LT :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (lt (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  (* | LPAREN :: e ->
      let temp = stackmc_prototype [] (find_paren e []) 0 rho in
      stackmc_prototype (temp :: acc)
        (drop e (List.length (find_paren e []) + 1))
        a rho *)
  | PAREN :: e -> stackmc_prototype acc e a rho
  | CONJ :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (get_bool (List.hd (List.tl acc)) && get_bool (List.hd acc))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | DISJ :: e ->
      if List.length acc >= a + 2 then
        stackmc_prototype
          ( Bool (get_bool (List.hd (List.tl acc)) || get_bool (List.hd acc))
          :: drop acc 2 )
          e a rho
      else raise Ill_Formed_Stack
  | NOT :: e ->
      if List.length acc >= a + 1 then
        stackmc_prototype
          (Bool (not (get_bool (List.hd acc))) :: drop acc 1)
          e a rho
      else raise Ill_Formed_Stack
  | IFTE :: e ->
      if List.length acc >= a + 3 then
        if get_bool (List.hd acc) = true then
          stackmc_prototype (List.hd (List.tl acc) :: drop acc 3) e a rho
        else
          stackmc_prototype
            (List.hd (List.tl (List.tl acc)) :: drop acc 3)
            e a rho
      else raise Ill_Formed_Stack
  | TUPLE e1 :: e ->
      if List.length acc >= a + e1 then
        stackmc_prototype
          (Tup (e1, tuple_stack_calc acc e1 []) :: drop acc e1)
          e a rho
      else raise Ill_Formed_Stack
  | PROJ (e1, e2) :: e ->
      if List.length acc >= a + 1 then
        stackmc_prototype
          (proj_stack_calc (List.hd acc) e1 :: drop acc 1)
          e a rho
      else raise Ill_Formed_Stack
  | [] ->
      if List.length acc = a + 1 then List.hd acc else raise Ill_Formed_Stack

(* Assuming that acc need not always be empty *)
(* if List.length acc = 1 then List.hd acc else raise Invalid_Expression *)

(* with
  | Failure _ -> raise Ill_Formed_Stack
  | Drop_number_exceeds_list -> raise Ill_Formed_Stack
  | Ill_Formed_Stack -> raise Ill_Formed_Stack
  | Invalid_type -> raise Invalid_type *)

let stackmc (acc : answer list) rho (op : opcode list) =
  stackmc_prototype acc op (List.length acc) rho

(* let a0 = N 2000

   let a1 = N 4

   let a2 = N 1200

   let a3 = N 50000

   let a4 = N 10

   let a5 = N 2

   let a6 = N 500

   let a7 = B true

   let a8 = B false

   let opcode1 = compile (Add (Mult (a1, a2), a3))

   let opcode2 = compile (Div (Negative a3, Mult (a4, a4)))

   let opcode3 = compile (Negative (Sub (Div (a6, a5), Mult (a1, a2))))

   let opcode4 = compile (Abs (Div (Negative a3, Rem (a4, a3))))

   let opcode5 =
   compile
    (IfThenElse
       ( Conjunction (a7, a8)
       , Add (Mult (a1, a2), a3)
       , Negative (Sub (Div (a6, a5), Mult (a1, a2))) )) *)
