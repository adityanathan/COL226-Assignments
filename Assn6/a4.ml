open A5_type

exception Variable_not_found

let get_tup_1 (x, _) = x

let get_tup_2 (_, x) = x

let rec find_variable (x : string) g =
  match g with
  | [] -> raise Variable_not_found
  | hd :: tl -> if x = get_tup_1 hd then get_tup_2 hd else find_variable x tl

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
  | Integer number -> Tint
  | Bool value -> Tbool
  | V var_name -> find_variable var_name g
	| Cmp e -> if hastype g e Tint then Tbool else raise Type_infer_invalid
  | And (e1, e2) ->
      if hastype g e1 Tbool && hastype g e2 Tbool then Tbool
      else raise Type_infer_invalid
  | Or (e1, e2) ->
      if hastype g e1 Tbool && hastype g e2 Tbool then Tbool
      else raise Type_infer_invalid
  | Equals (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tbool
      else raise Type_infer_invalid
  | GreaterTE (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tbool
      else raise Type_infer_invalid
  | LessTE (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tbool
      else raise Type_infer_invalid
  | GreaterT (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tbool
      else raise Type_infer_invalid
  | LessT (e1, e2) ->
      if hastype g e1 Tint && hastype g e2 Tint then Tbool
      else raise Type_infer_invalid
  | InParen e1 -> type_infer g e1
  | If_Then_Else (e1, e2, e3) ->
      if hastype g e1 Tbool && hastype g e3 (type_infer g e2) then
        type_infer g e2
      else raise Type_infer_invalid
  | Tuple (e1, e2) ->
      if e1 = List.length e2 then Ttuple (tuple_type_infer g e2)
      else raise Type_infer_invalid
  | Project ((e1, e2), e3) -> (
    match type_infer g e3 with
    | Ttuple b ->
        if e1 <= e2 && e2 = List.length b then List.hd (drop b (e1 - 1))
        else raise Type_infer_invalid
    | _ -> raise Type_infer_invalid )
  | Plus (e1, e2) ->
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
  | Lambda (str, exp, type_explicit) ->
      Tfunc (type_explicit, type_infer ((str, type_explicit) :: g) exp)
  | App (e1, e2) -> (
    match type_infer g e1 with
    | Tfunc (t1, t2) ->
        if hastype g e2 t1 then t2 else raise Type_infer_invalid
    | _ -> raise Type_infer_invalid )
	| RecursiveLambda (f_name, param, f_body, param_type, func_type) ->
			Tfunc (param_type, type_infer ((param, param_type) :: (f_name, Tfunc(param_type,func_type)) :: g) f_body)

and tuple_type_infer g a =
  match a with
  | [] -> []
  | hd :: tl -> type_infer g hd :: tuple_type_infer g tl

(* --------------------------------------------------------------------------------- *)

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
and hastype g e t =
  try
    match e with
    | Integer number -> Tint = t
    | Bool value -> Tbool = t
    | V var_name -> (
      try find_variable var_name g = t with Variable_not_found -> false )
		| Cmp e -> if hastype g e Tint then Tbool = t else false
		| And (e1, e2) ->
        if hastype g e1 Tbool && hastype g e2 Tbool then Tbool = t else false
    | Or (e1, e2) ->
        if hastype g e1 Tbool && hastype g e2 Tbool then Tbool = t else false
    | Equals (e1, e2) ->
        if hastype g e1 Tint && hastype g e2 Tint then Tbool = t else false
    | GreaterTE (e1, e2) ->
        if hastype g e1 Tint && hastype g e2 Tint then Tbool = t else false
    | LessTE (e1, e2) ->
        if hastype g e1 Tint && hastype g e2 Tint then Tbool = t else false
    | GreaterT (e1, e2) ->
        if hastype g e1 Tint && hastype g e2 Tint then Tbool = t else false
    | LessT (e1, e2) ->
        if hastype g e1 Tint && hastype g e2 Tint then Tbool = t else false
    | InParen e1 -> hastype g e1 t
    | If_Then_Else (e1, e2, e3) ->
        if hastype g e1 Tbool then hastype g e2 t && hastype g e3 t else false
    | Tuple (e1, e2) ->
        if List.length e2 = e1 then
          match t with Ttuple a -> match_list_of_types g e2 a | _ -> false
        else false
    (* | Project ((e1, e2), e3) ->
        if e1 <= e2 then
          match e3 with
          | Tuple (a, b) ->
              if e2 = a then hastype g (List.hd (drop b (e1 - 1))) t else false
          | _ -> false
        else false *)
    | Project ((e1, e2), e3) ->
        if e1 <= e2 then
          match type_infer g e3 with
          | Ttuple b ->
              if e2 = List.length b then List.hd (drop b (e1 - 1)) = t
              else false
          | _ -> false
        else false
    | Plus (e1, e2) ->
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
    | Lambda (str, exp, type_explicit) -> (
      match t with
      | Tfunc (t1, t2) ->
          if type_explicit = t1 then hastype ((str, t1) :: g) exp t2 else false
      | _ -> false )
		| RecursiveLambda (f_name, param, f_body, param_type, func_type) -> (
			match t with
			| Tfunc (t1, t2) ->
					if param_type = t1 && func_type = t2 then hastype ((param, t1) :: (f_name, t2) :: g) f_body t2 else false
			| _ -> false )
    | App (e1, e2) -> (
      match type_infer g e1 with
      | Tfunc (tau1, tau2) ->
          if type_infer g e2 = tau1 then tau2 = t else false
      | _ -> raise Type_infer_invalid )
  with _ -> false

and match_list_of_types g e t =
  if List.length e = List.length t then
    match (e, t) with
    | [], [] -> true
    | a :: b, c :: d ->
        if hastype g a c then match_list_of_types g b d else false
  else false
