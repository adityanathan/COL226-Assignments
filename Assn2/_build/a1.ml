open A0

let absa (x : int) = if x >= 0 then x else -x

type exptree =
  | N of int
  | Plus of exptree * exptree
  | Minus of exptree * exptree
  | Mult of exptree * exptree
  | Div of exptree * exptree
  | Rem of exptree * exptree
  | Neg of exptree
  | Abs of exptree

type opcode =
  | CONST of bigint
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | REM
  | ABS
  | UNARYMINUS

exception Invalid_Expression

let rec eval (exp : exptree) =
  match exp with
  | N number -> number
  | Plus (e1, e2) -> eval e1 + eval e2
  | Minus (e1, e2) -> eval e1 - eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) ->
      if eval e1 < 0 && eval e2 > 0 && eval e2 * (eval e1 / eval e2) <> eval e1
      then (eval e1 / eval e2) - 1
      else eval e1 / eval e2
  (* To make remainder always greater than 0 *)
  | Rem (e1, e2) ->
      if eval e1 < 0 && eval e2 > 0 && eval e2 * (eval e1 / eval e2) <> eval e1
      then (eval e1 mod eval e2) + eval e2
      else eval e1 mod eval e2
  | Neg e1 -> -1 * eval e1
  | Abs e1 -> absa (eval e1)
  | _ -> raise Invalid_Expression

let rec compile (exp : exptree) =
  match exp with
  | N number -> [CONST (mk_big number)]
  | Plus (e1, e2) -> compile e1 @ compile e2 @ [PLUS]
  | Minus (e1, e2) -> compile e1 @ compile e2 @ [MINUS]
  | Mult (e1, e2) -> compile e1 @ compile e2 @ [TIMES]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [DIV]
  | Rem (e1, e2) -> compile e1 @ compile e2 @ [REM]
  | Neg e1 -> compile e1 @ [UNARYMINUS]
  | Abs e1 -> compile e1 @ [ABS]
  | _ -> raise Invalid_Expression

exception Drop_number_exceeds_list

let rec drop l n =
  match l with
  | [] -> if n = 0 then [] else raise Drop_number_exceeds_list
  | hd :: tl -> if n = 0 then l else drop tl (n - 1)

exception Ill_Formed_Stack

let rec stackmc_prototype (acc : bigint list) (op : opcode list) (a : int) =
  try
    match op with
    | CONST num :: e -> stackmc_prototype (num :: acc) e a
    | PLUS :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            (add (List.hd acc) (List.hd (List.tl acc)) :: drop acc 2)
            e a
        else raise Ill_Formed_Stack
    | TIMES :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            (mult (List.hd acc) (List.hd (List.tl acc)) :: drop acc 2)
            e a
        else raise Ill_Formed_Stack
    | MINUS :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            (sub (List.hd (List.tl acc)) (List.hd acc) :: drop acc 2)
            e a
        else raise Ill_Formed_Stack
    | DIV :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            (div (List.hd (List.tl acc)) (List.hd acc) :: drop acc 2)
            e a
        else raise Ill_Formed_Stack
    | REM :: e ->
        if List.length acc >= a + 2 then
          stackmc_prototype
            (rem (List.hd (List.tl acc)) (List.hd acc) :: drop acc 2)
            e a
        else raise Ill_Formed_Stack
    | ABS :: e ->
        if List.length acc >= a + 1 then
          stackmc_prototype (abs (List.hd acc) :: drop acc 1) e a
        else raise Ill_Formed_Stack
    | UNARYMINUS :: e ->
        if List.length acc >= a + 1 then
          stackmc_prototype (minus (List.hd acc) :: drop acc 1) e a
        else raise Ill_Formed_Stack
    | [] ->
        if List.length acc = a + 1 then List.hd acc else raise Ill_Formed_Stack
    (* Assuming that acc need not always be empty *)
    (* if List.length acc = 1 then List.hd acc else raise Invalid_Expression *)
  with
  | Failure _ -> raise Ill_Formed_Stack
  | Drop_number_exceeds_list -> raise Ill_Formed_Stack
  | Ill_Formed_Stack -> raise Ill_Formed_Stack

let stackmc (acc : bigint list) (op : opcode list) =
  stackmc_prototype acc op (List.length acc)

(* let a0 = N 2000

let a1 = N 4

let a2 = N 1200

let a3 = N 50000

let a4 = N 10

let a5 = N 2

let a6 = N 500

let opcode1 = compile (Plus (Mult (a1, a2), a3))

let opcode2 = compile (Div (Neg a3, Mult (a4, a4)))

let opcode3 = compile (Neg (Minus (Div (a6, a5), Mult (a1, a2))))

let opcode4 = compile (Abs (Div (Neg a3, Rem (a4, a3)))) *)
