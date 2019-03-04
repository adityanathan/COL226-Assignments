open A0

exception Invalid_Expression

(* abstract syntax  *)
type exptree =
  | N of int
  (* Integer constant *)
  | B of bool
  (* Boolean constant *)
  | Var of string
  (* variable *)
  | Conjunction of exptree * exptree
  (* binary operators on booleans /\ *)
  | Disjunction of exptree * exptree
  (* binary operators on booleans \/ *)
  | Equals of exptree * exptree
  (* comparison operations on integers *)
  | GreaterTE of exptree * exptree
  (* comparison operations on integers *)
  | LessTE of exptree * exptree
  (* comparison operations on integers *)
  | GreaterT of exptree * exptree
  (* comparison operations on integers *)
  | LessT of exptree * exptree
  (* comparison operations on integers *)
  | InParen of exptree
  (* expressions using parenthesis *)
  | IfThenElse of exptree * exptree * exptree
  (* a conditional expression *)
  | Tuple of int * exptree list
  (* creating n-tuples (n >= 0) *)
  | Project of int * int * exptree list
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Plus of exptree * exptree
  (* binary operators on integers *)
  | Minus of exptree * exptree
  (* binary operators on integers *)
  | Mult of exptree * exptree
  (* binary operators on integers *)
  | Div of exptree * exptree
  (* binary operators on integers *)
  | Rem of exptree * exptree
  (* binary operators on integers *)
  | Nega of exptree
  | Not of exptree
  | Exp of exptree * exptree
  (* unary operators on booleans *)
  | Abs of exptree

(* unary operators on integers *)

type opcode =
  | CONSTI of bigint
  | CONSTB of bool
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | REM
  | EXP
  | ABS
  | UNARYMINUS
  | EQS
  | GTE
  | LTE
  | GT
  | LT
  | PAREN
  | CONJ
  | DISJ
  | NOT
  | IFTE
  | TUPLE of int
  | PROJ of int

type special_ret_type = Int of int | Bool of bool

exception Zero_exp_zero_error

let rec eval_exponent acc x y =
  match y with
  | 0 -> if x = 0 then raise Zero_exp_zero_error else 1
  | 1 -> acc * x
  | power -> eval_exponent acc * x x (y - 1)

let rec eval_b_exponent acc x y =
  match y with
  | NonNeg, [] ->
      if x = (NonNeg, []) then raise Zero_exp_zero_error else (NonNeg, [1])
  | NonNeg, [1] -> mult acc x
  | power -> eval_exponent (mult acc x) x (sub y (NonNeg, [1]))

let rec eval exp =
  match exp with
  | N number -> Int number
  | B value -> Bool value
  (* | Var var_name -> var_name           *)
  (* ----------------------------------------------------------WARNING *)
  | Conjunction (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a && b)
    | _, _ -> raise Invalid_Expression )
  | Disjunction (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a || b)
    | _, _ -> raise Invalid_Expression )
  | Equals (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a = b)
    | _, _ -> raise Invalid_Expression )
  | GreaterTE (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a >= b)
    | _, _ -> raise Invalid_Expression )
  | LessTE (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a <= b)
    | _, _ -> raise Invalid_Expression )
  | GreaterT (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a > b)
    | _, _ -> raise Invalid_Expression )
  | LessT (e1, e2) -> (
    match (eval e1, eval e2) with
    | Bool a, Bool b -> Bool (a < b)
    | _, _ -> raise Invalid_Expression )
  | InParen e1 -> eval e1
  | IfThenElse (e1, e2, e3) -> (
    match eval e1 with
    | Bool a -> if a = true then eval e2 else eval e3
    | _ -> raise Invalid_Expression )
  (* | Tuple (e1, e2) ->
  | Project (e1, e2) ->  *)
  | Plus (e1, e2) -> (
    match (eval e1, eval e2) with
    | Int a, Int b -> Int (a + b)
    | _, _ -> raise Invalid_Expression )
  | Minus (e1, e2) -> (
    match (eval e1, eval e2) with
    | Int a, Int b -> Int (a - b)
    | _, _ -> raise Invalid_Expression )
  | Mult (e1, e2) -> (
    match (eval e1, eval e2) with
    | Int a, Int b -> Int (a * b)
    | _, _ -> raise Invalid_Expression )
  | Div (e1, e2) -> (
    match (eval e1, eval e2) with
    | Int a, Int b ->
        if a < 0 && b > 0 && b * (a / b) <> a then Int ((a / b) - 1)
        else Int (a / b)
          (* Done to make sure Euclidean Division is satisfied *)
          (* To make remainder always greater than 0 *)
    | _, _ -> raise Invalid_Expression )
  | Rem (e1, e2) -> (
    match (eval e1, eval e2) with
    | Int a, Int b ->
        if a < 0 && b > 0 && b * (a / b) <> a then Int ((a mod b) + b)
        else Int (a mod b)
          (* Done to make sure Euclidean Division is satisfied *)
          (* To make remainder always greater than 0 *)
    | _, _ -> raise Invalid_Expression )
  | Exp (e1, e2) -> (
    match (eval e1, eval e2) with
    | Int a, Int b -> Int (eval_exponent 1 a b)
    | _, _ -> raise Invalid_Expression )
  | Nega e1 -> (
    match eval e1 with Int a -> Int (-1 * a) | _ -> raise Invalid_Expression )
  | Not e1 -> (
    match eval e1 with Bool a -> Bool (not a) | _ -> raise Invalid_Expression )
  | Abs e1 -> (
    match eval e1 with
    | Int a -> if a < 0 then Int (-1 * a) else Int a
    | _ -> raise Invalid_Expression )
  | _ -> raise Invalid_Expression

let rec compile exp =
  match exp with
  | N number -> [CONSTI (mk_big number)]
  | B value -> [CONSTB value]
  | Equals (e1, e2) -> compile e1 @ compile e2 @ [EQS]
  | GreaterTE (e1, e2) -> compile e1 @ compile e2 @ [GTE]
  | LessTE (e1, e2) -> compile e1 @ compile e2 @ [LTE]
  | GreaterT (e1, e2) -> compile e1 @ compile e2 @ [GT]
  | LessT (e1, e2) -> compile e1 @ compile e2 @ [LT]
  | Conjunction (e1, e2) -> compile e1 @ compile e2 @ [CONJ]
  | Disjunction (e1, e2) -> compile e1 @ compile e2 @ [DISJ]
  | InParen e1 -> [PAREN] @ compile e1 @ [PAREN]
  | Plus (e1, e2) -> compile e1 @ compile e2 @ [PLUS]
  | Minus (e1, e2) -> compile e1 @ compile e2 @ [MINUS]
  | Mult (e1, e2) -> compile e1 @ compile e2 @ [TIMES]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [DIV]
  | Rem (e1, e2) -> compile e1 @ compile e2 @ [REM]
  | Abs e1 -> compile e1 @ [ABS]
  | Nega e1 -> compile e1 @ [UNARYMINUS]
  | Not e1 -> compile e1 @ [NOT]
  | Exp (e1, e2) -> compile e1 @ compile e2 @ [EXP]
  | IfThenElse (e1, e2, e3) -> compile e3 @ compile e2 @ compile e1 @ [IFTE]
  (* | Tuple (e1, e2) ->
  | Project (e1, e2) ->  *)
  | _ -> raise Invalid_Expression

exception Drop_number_exceeds_list

exception Invalid_type

let rec drop l n =
  match l with
  | [] -> if n = 0 then [] else raise Drop_number_exceeds_list
  | hd :: tl -> if n = 0 then l else drop tl (n - 1)

type bigint_ret_type = BInt of bigint | Bool of bool

let get_int a = match a with BInt e1 -> e1 | _ -> raise Invalid_type

let get_bool a = match a with Bool e1 -> e1 | _ -> raise Invalid_type

exception Ill_Formed_Stack

let rec find_paren list accumulator =
  match list with
  | PAREN :: e -> accumulator
  | a :: b -> find_paren b accumulator @ [a]
  | [] -> raise Ill_Formed_Stack

let rec stackmc_prototype (acc : bigint_ret_type list) (op : opcode list)
    (a : int) =
  try
    match op with
    | CONSTI (num : bigint) :: e -> stackmc_prototype (BInt num :: acc) e a
    | CONSTB (value : bool) :: e -> stackmc_prototype (Bool value :: acc) e a
    | PLUS :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( BInt
                (add (get_int (List.hd acc)) (get_int (List.hd (List.tl acc))))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | TIMES :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( BInt
                (mult (get_int (List.hd acc)) (get_int (List.hd (List.tl acc))))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | MINUS :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( BInt
                (sub (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | EXP :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( BInt
                (eval_b_exponent
                   (NonNeg, [1])
                   (get_int (List.hd (List.tl acc)))
                   (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | DIV :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( BInt
                (div (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | REM :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( BInt
                (rem (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | ABS :: e ->
        if List.length acc >= a + 1 then
          stackmc_prototype
            (BInt (abs (get_int (List.hd acc))) :: drop acc 1)
            e a
        else raise Ill_Formed_Stack
    | UNARYMINUS :: e ->
        if List.length acc >= a + 1 then
          stackmc_prototype
            (BInt (minus (get_int (List.hd acc))) :: drop acc 1)
            e a
        else raise Ill_Formed_Stack
    | EQS :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool
                (eq (get_int (List.hd acc)) (get_int (List.hd (List.tl acc))))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | GTE :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool
                (geq (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | LTE :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool
                (leq (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | GT :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool
                (gt (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | LT :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool
                (lt (get_int (List.hd (List.tl acc))) (get_int (List.hd acc)))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | PAREN :: e ->
        let temp = stackmc_prototype [] (find_paren e []) 0 in
        stackmc_prototype (temp :: acc)
          (drop e (List.length (find_paren e []) + 1))
          a
    | CONJ :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool (get_bool (List.hd (List.tl acc)) && get_bool (List.hd acc))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | DISJ :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            ( Bool (get_bool (List.hd (List.tl acc)) || get_bool (List.hd acc))
            :: drop acc 2 )
            e a
        else raise Ill_Formed_Stack
    | NOT :: e ->
        if List.length acc >= a + 1 then
          stackmc_prototype
            (Bool (not (get_bool (List.hd acc))) :: drop acc 1)
            e a
        else raise Ill_Formed_Stack
    | IFTE :: e ->
        if List.length acc >= a + 3 then
          if get_bool (List.hd acc) = true then
            stackmc_prototype (List.hd (List.tl acc) :: drop acc 3) e a
          else
            stackmc_prototype
              (List.hd (List.tl (List.tl acc)) :: drop acc 3)
              e a
        else raise Ill_Formed_Stack
    (* Tuple and Projection pending *)
    | [] ->
        if List.length acc = a + 1 then List.hd acc else raise Ill_Formed_Stack
    (* Assuming that acc need not always be empty *)
    (* if List.length acc = 1 then List.hd acc else raise Invalid_Expression *)
  with
  | Failure _ -> raise Ill_Formed_Stack
  | Drop_number_exceeds_list -> raise Ill_Formed_Stack
  | Ill_Formed_Stack -> raise Ill_Formed_Stack
  | Invalid_type -> raise Invalid_type

let stackmc (acc : bigint_ret_type list) (op : opcode list) =
  stackmc_prototype acc op (List.length acc)

(* let a0 = N 2000

let a1 = N 4

let a2 = N 1200

let a3 = N 50000

let a4 = N 10

let a5 = N 2

let a6 = N 500

let a7 = B true

let a8 = B false

let opcode1 = compile (Plus (Mult (a1, a2), a3))

let opcode2 = compile (Div (Nega a3, Mult (a4, a4)))

let opcode3 = compile (Nega (Minus (Div (a6, a5), Mult (a1, a2))))

let opcode4 = compile (Abs (Div (Nega a3, Rem (a4, a3))))

let opcode5 =
  compile
    (IfThenElse
       ( Conjunction (a7, a8)
       , Plus (Mult (a1, a2), a3)
       , Nega (Minus (Div (a6, a5), Mult (a1, a2))) )) *)
