open A1

exception Variable_not_found

let get_tup_1 (x, _) = x

let get_tup_2 (_, x) = x

let rec find_variable x g =
  match g with
  | [] -> Variable_not_found
  | hd :: tl -> if hd = get_tup_1 x then get_tup_2 x else find_variable x tl

let rec match_list_of_types g (e : exptree list) (t : exptype list) =
  if List.length e = List.length t then
    match (e, t) with
    | [], [] -> true
    | a :: b, c :: d ->
        if hastype g a c then match_list_of_types g b d else false
  else false

exception Drop_number_exceeds_list

let rec drop l n =
  match l with
  | [] -> if n = 0 then [] else raise Drop_number_exceeds_list
  | hd :: tl -> if n = 0 then l else drop tl (n - 1)

(* To check if there are multiple parallel definitions in gamma_dash for parallel case in yields function *)
let rec check_overlap x y =
  match y with
  | [] -> false
  | hd :: tl -> if hd = x then true else check_overlap x tl

let rec check_list_overlap x =
  match x with
  | [] -> false
  | hd :: tl -> if check_overlap hd tl then true else check_list_overlap tl

exception Type_infer_invalid

let rec type_infer g e =
  match e with
  | N number -> Tint
  | B value -> Tbool
  | Var var_name -> (
    try find_variable e g with Variable_not_found -> raise Type_infer_invalid )
  | Conjunction (e1, e2) ->
      if hastype g e1 Tbool && hastype g e2 TBool then TBool
      else raise Type_infer_invalid
  | Disjunction (e1, e2) ->
      if hastype g e1 Tbool && hastype g e2 TBool then TBool
      else raise Type_infer_invalid
  | Equals (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool
      else raise Type_infer_invalid
  | GreaterTE (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool
      else raise Type_infer_invalid
  | LessTE (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool
      else raise Type_infer_invalid
  | GreaterT (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool
      else raise Type_infer_invalid
  | LessT (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool
      else raise Type_infer_invalid
  | InParen e1 -> type_infer g e1
  | IfThenElse (e1, e2, e3) ->
      if hastype g e1 Tbool then type_infer g e2 && type_infer g e3
      else raise Type_infer_invalid
  | Tuple (e1, e2) ->
      if hastype g e1 Tint then
        match t with Ttuple a -> a | _ -> raise Type_infer_invalid
      else raise Type_infer_invalid
  | Project ((e1, e2), e3) ->
      if hastype g e1 Tint && hastype g e2 Tint then
        match e3 with
        | Tuple (a, b) -> type_infer g (List.hd (drop e3 (e1 - 1)))
        | _ -> raise Type_infer_invalid
      else raise Type_infer_invalid
  | Add (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tint
      else raise Type_infer_invalid
  | Sub (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tint
      else raise Type_infer_invalid
  | Mult (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tint
      else raise Type_infer_invalid
  | Div (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tint
      else raise Type_infer_invalid
  | Rem (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tint
      else raise Type_infer_invalid
  | Negative e1 -> if hastype g e1 Tint then Tint else raise Type_infer_invalid
  | Not e1 -> if hastype g e1 Tbool then Tbool else raise Type_infer_invalid
  | Abs e1 -> if hastype g e1 Tint then Tint else raise Type_infer_invalid
  | Let (e1, e2) -> type_infer (type_infer_list g d) :: g e

let rec type_infer_list g d =
  match d with
  | Simple (e1, e2) -> (e1, type_infer g e2)
  | Sequence e1 ->
      let e = List.hd e1 in
      let temp = (get_tup_1 e1, type_infer (get_tup_2 e1)) in
      temp :: type_infer_list (temp :: g) (List.tl e1)
  | Parallel e1 ->
      let e = List.hd e1 in
      let temp = (get_tup_1 e1, type_infer (get_tup_2 e1)) in
      temp :: type_infer_list g (List.tl e1)
  | Local (d1, d2) -> type_infer_list (type_infer_list g d1 :: g) d2

(* --------------------------------------------------------------------------------- *)

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t =
  match e with
  | N number -> Tint = t
  | B value -> Tbool = t
  | Var var_name -> (
    try find_variable e g = t with Variable_not_found -> false )
  | Conjunction (e1, e2) ->
      if hastype g e1 Tbool && hastype g e2 TBool then TBool = t else false
  | Disjunction (e1, e2) ->
      if hastype g e1 Tbool && hastype g e2 TBool then TBool = t else false
  | Equals (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool = t else false
  | GreaterTE (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool = t else false
  | LessTE (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool = t else false
  | GreaterT (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool = t else false
  | LessT (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 TInt then TBool = t else false
  | InParen e1 -> hastype g e1 t
  | IfThenElse (e1, e2, e3) ->
      if hastype g e1 Tbool then hastype g e2 t && hastype g e3 t else false
  | Tuple (e1, e2) ->
      if hastype g e1 Tint then
        match t with Ttuple a -> list_of_types g e2 a | _ -> false
      else false
  | Project ((e1, e2), e3) ->
      if hastype g e1 Tint && hastype g e2 Tint then
        match e3 with
        | Tuple (a, b) -> hastype g (List.hd (drop e3 (e1 - 1))) t
        | _ -> false
      else false
  | Add (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then t = Tint else false
  | Sub (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then t = Tint else false
  | Mult (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then t = Tint else false
  | Div (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then t = Tint else false
  | Rem (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then t = Tint else false
  | Negative e1 -> if hastype g e1 Tint then t = Tint else false
  | Not e1 -> if hastype g e1 Tbool then t = Tbool else false
  | Abs e1 -> if hastype g e1 Tint then t = Tint else false
  | Let (d, e) -> hastype (type_infer_list g d) :: g e t
  | FunctionAbstraction (str, exp) -> (
    match t with
    | Tfunc (t1, t2) -> hastype ((str, t1) :: g) exp t2
    | _ -> false )
  | FunctionCall (e1, e2) -> hastype g e1 Tfunc (type_infer g e2, t)

(* yields : ((string * exptype) list) -> definition -> ((string * exptype) list) -> bool *)
let rec yields g d g_dash =
  match d with
  | Simple (e1, e2) ->
      if e1 = get_tup_1 g_dash then hastype g e2 (get_tup_2 g_dash) else false
  | Sequence e1 ->
      let e = List.hd e1 in
      let e_dash = List.hd g_dash in
      if yields g e e_dash then
        yields (e_dash :: g) (List.tl e1) (List.tl g_dash)
      else false
  | Parallel e1 ->
      if check_list_overlap e1 then false
      else
        let e = List.hd e1 in
        let e_dash = List.hd g_dash in
        if yields g e e_dash then yields g (List.tl e1) (List.tl g_dash)
        else false
  | Local (d1, d2) -> yields (type_infer_list g d1) :: g d2 g_dash
