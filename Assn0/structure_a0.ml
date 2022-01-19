open Signature_a0

module A0 : BigInt = struct
  type sign = Neg | NonNeg

  type bigint = sign * int list

  (* elements of the list are between 0 and 9 *)
  (* MSB first *)
  (* No unnecessary leading zeros *)
  (* 0 is considered NonNeg *)
  (* x and y have to reversed before the arguments can be passed *)

  let rev x = List.rev x

  let hd x = List.hd x

  let tl x = List.tl x

  let len x = List.length x

  let get_sign ((s, _) : bigint) = s

  let get_list ((_, l) : bigint) = l

  let rec take l n =
    match l with
    | [] -> []
    | hd :: tl -> if n = 0 then [] else hd :: take tl (n - 1)

  let rec drop l n =
    match l with [] -> [] | hd :: tl -> if n = 0 then l else drop tl (n - 1)

  let rec trim (x : bigint) =
    match x with
    | _, [] -> ((NonNeg, []) : bigint)
    | s, 0 :: t1 -> (trim (s, t1) : bigint)
    | _ -> (x : bigint)

  let rec trim_list x =
    match x with [] -> [] | 0 :: t1 -> trim_list t1 | _ -> x

  (* Assuming mk_big_prototype recieves only NonNeg integers *)
  let rec mk_big_prototype x acc =
    match x with 0 -> acc | _ -> mk_big_prototype (x / 10) ((x mod 10) :: acc)

  (* Remember that - is an infix function and therefore to pass negative values enclose it with brackets *)
  (* Like this (-2) *)
  let mk_big x =
    if x < 0 then ((Neg, mk_big_prototype (-x) []) : bigint)
    else ((NonNeg, mk_big_prototype x []) : bigint)

  let print_num (x : bigint) =
    let x = trim x in
    match x with
    | Neg, l -> String.concat "" ("-" :: List.map string_of_int l)
    | NonNeg, l ->
        if l <> [] then String.concat "" (List.map string_of_int l) else "0"

  (* let get_num (x : bigint) = int_of_string (print_num x) *)

  (* minus 0 will not give (Neg,[]). *)
  let minus (x : bigint) =
    let x = trim x in
    match x with
    | Neg, e -> ((NonNeg, e) : bigint)
    | NonNeg, e ->
        if e <> [] then ((Neg, e) : bigint) else ((NonNeg, e) : bigint)

  (* | NonNeg, e -> ((Neg, e) : bigint) *)

  let abs ((_, x) : bigint) : bigint = trim (NonNeg, x)

  let rec shift_prototype (x : int list) (acc : int list) =
    match x with
    | [] -> []
    | [hd] -> acc @ [hd] @ [0]
    | hd :: tl -> shift_prototype tl (acc @ [hd])

  let rec shift (x : int list) i =
    match i with
    | 0 -> x
    | 1 -> shift_prototype x []
    | _ -> shift (shift_prototype x []) (i - 1)

  let rec eq_prototype x y =
    if len x = len y then
      match (x, y) with
      | [], [] -> true
      | h1 :: t1, h2 :: t2 -> if h1 = h2 then eq_prototype t1 t2 else false
    else false

  let eq (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    if get_sign x = get_sign y then eq_prototype (get_list x) (get_list y)
    else false

  let rec gt_prototype x y =
    if len x > len y then true
    else if len x < len y then false
    else
      match (x, y) with
      | [], [] -> false
      | h1 :: t1, h2 :: t2 ->
          if h1 > h2 then true
          else if h1 = h2 then gt_prototype t1 t2
          else false

  (* | [], [] -> false *)

  let gt (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    if eq x y then false
    else
      match (x, y) with
      | (Neg, _), (NonNeg, _) -> false
      | (NonNeg, _), (Neg, _) -> true
      | (Neg, l1), (Neg, l2) -> not (gt_prototype l1 l2)
      | (NonNeg, l1), (NonNeg, l2) -> gt_prototype l1 l2

  let rec lt_prototype x y =
    if len x > len y then false
    else if len x < len y then true
    else
      match (x, y) with
      | [], [] -> false
      | h1 :: t1, h2 :: t2 ->
          if h1 < h2 then true
          else if h1 = h2 then lt_prototype t1 t2
          else false

  (* | [], [] -> false *)

  let lt (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    if eq x y then false
    else
      match (x, y) with
      | (Neg, _), (NonNeg, _) -> true
      | (NonNeg, _), (Neg, _) -> false
      | (Neg, l1), (Neg, l2) -> not (lt_prototype l1 l2)
      | (NonNeg, l1), (NonNeg, l2) -> lt_prototype l1 l2

  let geq (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    if eq x y then true else if gt x y then true else false

  let leq (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    if eq x y then true else if lt x y then true else false

  let rec add_prototype x y acc carry =
    match (x, y) with
    | [], [] -> carry :: acc
    | [], h1 :: t1 ->
        if h1 + carry > 9 then add_prototype [] t1 ((h1 + carry - 10) :: acc) 1
        else add_prototype [] t1 ((h1 + carry) :: acc) 0
    | h1 :: t1, [] ->
        if h1 + carry > 9 then add_prototype t1 [] ((h1 + carry - 10) :: acc) 1
        else add_prototype t1 [] ((h1 + carry) :: acc) 0
    | h1 :: t1, h2 :: t2 ->
        if h1 + h2 + carry > 9 then
          add_prototype t1 t2 ((h1 + h2 + carry - 10) :: acc) 1
        else add_prototype t1 t2 ((h1 + h2 + carry) :: acc) 0

  let rec sub_prototype x y acc carry =
    match (x, y) with
    | [], [] -> acc
    | h1 :: t1, [] ->
        if h1 - carry < 0 then sub_prototype t1 [] ((h1 - carry + 10) :: acc) 1
        else sub_prototype t1 [] ((h1 - carry) :: acc) 0
    | h1 :: t1, h2 :: t2 ->
        if h1 - h2 - carry < 0 then
          sub_prototype t1 t2 ((h1 - h2 - carry + 10) :: acc) 1
        else sub_prototype t1 t2 ((h1 - h2 - carry) :: acc) 0

  let rec add (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    match (x, y) with
    | (Neg, l1), (Neg, l2) ->
        trim ((Neg, add_prototype (rev l1) (rev l2) [] 0) : bigint)
    | (Neg, l1), (NonNeg, l2) -> (sub y (abs x) : bigint)
    | (NonNeg, l1), (Neg, l2) -> (sub x (abs y) : bigint)
    | (NonNeg, l1), (NonNeg, l2) ->
        trim ((NonNeg, add_prototype (rev l1) (rev l2) [] 0) : bigint)

  and sub (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    if eq x y then (NonNeg, [])
    else if gt (abs x) (abs y) then
      match (x, y) with
      | (NonNeg, l1), (NonNeg, l2) ->
          trim (NonNeg, sub_prototype (rev l1) (rev l2) [] 0)
      | (Neg, l1), (NonNeg, l2) -> minus (add (abs x) y)
      | (NonNeg, l1), (Neg, l2) -> add x (abs y)
      | (Neg, l1), (Neg, l2) -> trim (Neg, sub_prototype (rev l1) (rev l2) [] 0)
    else
      match (x, y) with
      | (NonNeg, l1), (NonNeg, l2) ->
          trim (Neg, sub_prototype (rev l2) (rev l1) [] 0)
      | (Neg, l1), (NonNeg, l2) -> minus (add (minus x) y)
      | (NonNeg, l1), (Neg, l2) -> add x (minus y)
      | (Neg, l1), (Neg, l2) ->
          trim (NonNeg, sub_prototype (rev l2) (rev l1) [] 0)

  (* y has to be a single digit *)
  (* assuming x is reversed *)
  let rec mult_single (x : int list) (y : int) (acc : int list) (carry : int) =
    match x with
    | [] -> carry :: acc
    | hd :: tl ->
        mult_single tl y
          ((((hd * y) + carry) mod 10) :: acc)
          (((hd * y) + carry) / 10)

  let rec mult_prototype (x : int list) (y : int list) (acc : int list)
      (i : int) =
    match y with
    | [] -> acc
    (* | [hd] -> get_list (add (NonNeg, mult_single x hd [] 0) (NonNeg, acc)) *)
    | hd :: tl ->
        mult_prototype x tl
          (get_list
             (add (NonNeg, shift (mult_single (rev x) hd [] 0) i) (NonNeg, acc)))
          (i + 1)

  let mult x y =
    let x, y = (trim x, trim y) in
    match (x, y) with
    | (Neg, l1), (Neg, l2) ->
        trim (NonNeg, mult_prototype (get_list x) (rev (get_list y)) [] 0)
    | (NonNeg, l1), (NonNeg, l2) ->
        trim (NonNeg, mult_prototype (get_list x) (rev (get_list y)) [] 0)
    | (NonNeg, l1), (Neg, l2) ->
        trim (Neg, mult_prototype (get_list x) (rev (get_list y)) [] 0)
    | (Neg, l1), (NonNeg, l2) ->
        trim (Neg, mult_prototype (get_list x) (rev (get_list y)) [] 0)

  let rec div_single x y quotient =
    let a = (NonNeg, x) in
    let b = (NonNeg, y) in
    if lt a b then quotient
    else if eq a b then quotient + 1
    else div_single (get_list (sub a b)) y (quotient + 1)

  let rec div_prototype x y quotient i =
    let a = (NonNeg, x) in
    let b = (NonNeg, y) in
    let one = mk_big 1 in
    let q = (NonNeg, quotient) in
    if lt a b && i = -1 then quotient
    else if lt a b && i = 0 then div_prototype x y (quotient @ [0]) (i - 1)
    else if eq a b then get_list (add q one)
    else
      let head = take x (len x - i) in
      let tail = drop x (len x - i) in
      let rem =
        get_list
          (sub (NonNeg, head)
             (mult (NonNeg, trim_list [div_single head y 0]) b))
      in
      let q_single = [div_single head y 0] in
      div_prototype (rem @ tail) y (quotient @ q_single) (i - 1)

  exception Divison_by_zero_error

  let div (x : bigint) (y : bigint) =
    let x, y = (trim x, trim y) in
    match (x, y) with
    | (_, l1), (_, []) -> raise Divison_by_zero_error
    | (NonNeg, l1), (NonNeg, l2) ->
        trim ((NonNeg, div_prototype l1 l2 [] (len (get_list x) - 1)) : bigint)
    | (NonNeg, l1), (Neg, l2) ->
        trim ((Neg, div_prototype l1 l2 [] (len (get_list x) - 1)) : bigint)
    | (Neg, l1), (NonNeg, l2) ->
        trim ((Neg, div_prototype l1 l2 [] (len (get_list x) - 1)) : bigint)
    | (Neg, l1), (Neg, l2) ->
        trim ((NonNeg, div_prototype l1 l2 [] (len (get_list x) - 1)) : bigint)

  let rem x y =
    let x, y = (trim x, trim y) in
    sub x (mult y (div x y))
end
