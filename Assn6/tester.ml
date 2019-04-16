#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;
open A4;;

(* To RUN :- Just use #use "tester.ml" in the terminal *)
(* The gamma table to be used in the hastype *)
let g = [("X", Tint); ("Y", Tbool); ("Z", Tfunc(Tint, Tbool)); ("W", Tfunc(Tbool, Tint))];;

let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s = A3.def_parser A2.read (Lexing.from_string s) ;;

(* For printing type, You might want to add pattern matchings if you have defined extra constructors *)
let rec type_to_string t = match t with
| Tint -> "Tint"
| Tbool -> "Tbool"
| Tfunc(t1, t2) -> "Tfunc(" ^ (type_to_string t1) ^ "-->" ^ (type_to_string t2) ^ ")"
| Ttuple(tl) -> let tlist = List.map type_to_string tl in
                let s = "Ttuple("^(List.hd tlist) in let x = List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.tl tlist) in s ^ x ^ ")"
| Tunit -> "()"

(* Takes an exp and checks if it has type t. The answer is cross-checked with user provided answer *)
let test_exp s tup = 
try
    let tree = exp_parser s and 
    type_check = fst tup and 
    ans = snd tup in
    let result = hastype g tree type_check in
    if(result = ans) then Printf.printf"\n\"%s\" with type \"%s\"-> Test Case Passed\n" s (type_to_string type_check)
    else Printf.printf "\n\"%s\" with type \"%s\"-> Test Case Failed (%B)\n " s (type_to_string type_check) result

with e -> Printf.printf"\n\"%s\" -> Exception Encountered ---> %s\n" s (Printexc.to_string e)

let test_def s tup = 
try
    let tree = def_parser s and 
    type_check = fst tup and 
    ans = snd tup in
    let result = yields g tree type_check in
    if(result = ans) then Printf.printf"\n\"%s\"-> Test Case Passed\n" s 
    else Printf.printf "\n\"%s\"-> Test Case Failed (%B)\n " s  result

with e -> Printf.printf"\"%s\" -> Exception Encountered ---> %s\n" s (Printexc.to_string e)

let exp_tester = Hashtbl.create 100;;

Hashtbl.add exp_tester "\\X.X" (Tfunc(Ttuple([Tint; Tbool]), Ttuple([Tint; Tbool])), true);
Hashtbl.add exp_tester "let def A = Z(X) in W(A) end" (Tint, true);
Hashtbl.add exp_tester "\\M.(W(Z(M)))" (Tfunc(Tint, Tint), true);
Hashtbl.add exp_tester "proj(2, 3) ((X, T), (let def X = T /\\ F in not X end, \\X.(proj(1,2) X)), (3, 5))" (Ttuple([Tbool; Tfunc(Ttuple([Tint; Tbool]), Tint)]), false);
Hashtbl.add exp_tester "proj(1,2) (if Y then (1, 2) else (2, 3) fi)" (Tint, true);
Hashtbl.add exp_tester "if Y then (1, 2) else (T, 4) fi" (Ttuple([Tint; Tint]), false);
Hashtbl.add exp_tester "let def A = W(X) in A + 4 end" (Tint, false);
Hashtbl.add exp_tester "proj(2,4)proj(2,2)(T, (8,(if 5>6 then (\\Fs.W) else (\\Fs.W) fi),T,4) )" (Tfunc(Tint, Tfunc(Tbool, Tint)), true);


Printf.printf"\n----------------------------------------\n";;
Printf.printf"\nChecking expressions : \n\n";;
Hashtbl.iter test_exp exp_tester;;
Printf.printf"\n----------------------------------------\n";;

let def_tester = Hashtbl.create 100;;

Hashtbl.add def_tester "def U = X" ([("U", Tint)], true);
Hashtbl.add def_tester "def U = X ; def V = W(Y)" ([("U", Tint); ("V", Tint)], true);
Hashtbl.add def_tester "def V = W(X)" ([("V", Tint)], false);

Printf.printf"\nChecking definitions : \n\n";;
Hashtbl.iter test_def def_tester;;
Printf.printf"\n----------------------------------------\n";;