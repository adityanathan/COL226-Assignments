type procedure_type = Proc of string * int * ((string*int) list) * (string list) * (string list) * (string list)

let procedure_database = [Proc("Main", 0, [], ["a";"b";"c"], [], ["P";"Q";"Main"]);
                          Proc("P", 1, [("b",0) ; ("c",0)], ["z";"a"], ["x";"y"], ["P";"Q";"R";"S"]);
                          Proc("Q", 1, [("a",0) ; ("c",0)], ["x";"b"], ["z";"w"], ["P";"Q";"T";"U"]);
                          Proc("R", 2, [("a",1);("c",0);("x",1);("y",1);("z",1)], ["j";"b"], ["w";"i"], ["P";"Q";"V";"R";"S"]);
                          Proc("S", 2, [("a",1);("b",0);("x",1);("y",1);("z",1)], ["m";"n"], ["c";"k"], ["P";"Q";"R";"S"]);
                          Proc("T", 2, [("b",1);("c",0);("w",1);("x",1);("z",1)], ["i";"j"], ["a";"y"], ["P";"Q";"W";"T";"U"]);
                          Proc("U", 2, [("a",0);("b",1);("w",1);("x",1)], ["p";"g"], ["c";"z"], ["P";"Q";"T";"U"]);
                          Proc("V", 3, [("a",1);("b",2);("i",2);("j",2);("w",2);("x",1);("y",1);("z",1)], ["c"], ["m";"n"], ["P";"Q";"V";"R";"S"]);
                          Proc("W", 3, [("a",2);("b",1);("c",0);("f",2);("i",2);("w",1);("x",1);("y",2);("z",1)], ["j";"h"], ["m";"p"], ["P";"Q";"W";"T";"U"])]

type var_answer = Numa of int | Undefined

type assn_type = Num of int | Str of string

type frame = procedure_type * ((string * var_answer) list)

type frame_pointer = Null | Pointer of frame ref

type command = Call of string*(assn_type list) | Display | Set of (string * assn_type) | Return | Exit

exception Attempt_to_dereference_null_pointer
exception Attempt_to_assign_null_pointer

let ( !^ ) = function
    | Null -> raise Attempt_to_dereference_null_pointer
    | Pointer r -> !r;;

let ( ^:= ) p v =
    match p with
    | Null -> raise Attempt_to_assign_null_pointer
    | Pointer r -> r := v;;

let new_pointer x = Pointer (ref x);;

exception Procedure_not_found
exception Variable_not_found
exception Insufficient_Parameter_values
exception Index_out_of_bounds

let rec find_proc x l = match l with
  | Proc(a,b,c,d,e,f)::tl -> if a = x then Proc(a,b,c,d,e,f) else find_proc x tl
  | [] -> raise Procedure_not_found

let rec extract_var_from_table a b = match b with
  | (c,d)::tl -> if a=c then d else extract_var_from_table a tl
  | [] -> raise Variable_not_found

let rec substitute_variable a b = match a with
  | Num(f) -> Numa(f)
  | Str(f) -> extract_var_from_table f b

let rec replace_global_table global_list disp_reg name rep_val = match (extract_var_from_table name global_list) with
  | level -> (match !^(List.nth (disp_reg) level) with
                      (Proc(a,b,c,d,e,f),tabl) ->
                      let _ = ((List.nth (disp_reg) level) ^:= (Proc(a,b,c,d,e,f),replace_var_from_table name rep_val tabl c disp_reg)) in ())

and replace_var_from_table name rep_val tabl global_list disp_reg= match tabl with
  | (c,d)::tl -> if name=c then [(name,rep_val)]@tl else (c,d)::(replace_var_from_table name rep_val tl global_list disp_reg)
  | [] -> let _ = (replace_global_table global_list disp_reg name rep_val) in tabl

let rec match_global_var a display_reg = match a with
    (c,d)::tl -> (match !^(List.nth (display_reg) d) with (_,table) -> (c,extract_var_from_table c table)::match_global_var tl display_reg)
  | [] -> []

let rec match_local_var d = match d with
    hd::tl -> (hd,Undefined)::match_local_var tl
  | [] -> []

let rec match_param_var name value table = if List.length name = List.length value then
    (match (name,value) with
       (hd1::tl1,Num(hd2)::tl2) -> (hd1,Numa(hd2))::(match_param_var tl1 tl2 table)
     | (hd1::tl1,Str(hd2)::tl2) -> (hd1,extract_var_from_table hd2 table)::(match_param_var tl1 tl2 table)
     | ([],[]) -> [])
  else raise Insufficient_Parameter_values

let rec replace_elem e l count = (if List.length l > count then
    (match l with hd::tl -> if count = 0 then [e]@tl else hd::replace_elem e tl (count-1))
  else if List.length l = count then e::l
  else raise Index_out_of_bounds)

let rec exist a b = match b with
  | hd::tl -> if hd=a then true else exist a tl
  | [] -> false

let call_update callee param_list call_stack display_reg du= let callee_pr = find_proc callee procedure_database in
  let caller_pr_var_table = (match !^(List.hd call_stack) with (a,b) -> b) in
  let caller_proc_access = (match !^(List.hd call_stack) with (Proc(_,_,_,_,_,t),b) -> t) in
  match callee_pr with
    | Proc(a,b,c,d,e,f) -> if exist a caller_proc_access then (let a = new_pointer (callee_pr, (match_param_var e param_list caller_pr_var_table)@(match_local_var d)) in
                          let call_stack = a::call_stack in
                          let disp = replace_elem (List.hd call_stack) display_reg b in
                          let dump = (List.nth display_reg b)::du in
                                                               (call_stack,dump,disp))
      else raise Procedure_not_found

let rec print_tabl t = match t with
  |  (name, Numa(value))::tl -> Printf.printf "%s = %i, " name value; print_tabl tl;
  |  (name, Undefined)::tl -> Printf.printf "%s = Undefined, " name; print_tabl tl;
  | [] -> Printf.printf "\n";
;;

let rec print_proc t = match t with
  | hd::tl -> Printf.printf "%s, " hd; print_proc tl;
  | [] -> Printf.printf "\n"; Printf.printf "\n";
;;
let rec print_frame a displ_reg = match a with
    (Proc(a,b,c,d,e,f),tabl) -> Printf.printf "Frame: \n"; Printf.printf "Name: %s, Level: %i, " a b; Printf.printf "\n";
    Printf.printf "List of accessible variables: "; Printf.printf "\n";
    print_tabl tabl; print_tabl (match_global_var c displ_reg);
    Printf.printf "List of accessible procedures: "; print_proc f;
    ;;

let rec print_frame_disp a = match a with
    (Proc(a,b,c,d,e,f),tabl) ->Printf.printf "Display Register: \n"; Printf.printf "Name: %s, Level: %i, " a b; Printf.printf "\n";
    ;;

let rec print_stack c_stack disp_reg = try
    match c_stack with
  | hd::tl -> print_frame !^hd disp_reg; print_stack tl disp_reg;
  | [] -> Printf.printf "\n";
with
Attempt_to_dereference_null_pointer -> (match c_stack with hd::tail -> print_stack tail disp_reg
                                                          | [] -> Printf.printf "\n")

let rec print_disp di_reg = try
    match di_reg with
  | hd::tl -> print_frame_disp !^hd; print_disp tl;
  | [] -> Printf.printf "\n";
with
Attempt_to_dereference_null_pointer -> (match di_reg with hd::tail -> print_disp tail
                                                                             | [] -> Printf.printf "\n")
