#directory "_build";; (* Consider this folder when looking for files *)
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
#load "a5_krivine.cmo";;
(* #load "a5_secd.cmo";; *)
open A2;;
open A3;;
open A4;;
open A5_krivine;;
(* open A5_secd;; *)

exception Not_implemented;;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;

let kr_g = [("X", Integer 2)];;

let input_expr= A3.exp_parser A2.read (Lexing.from_string e);;
let kmc e = krivine_machine (input_expr) kr_g;;

kmc "1+2*3";;

kmc "if T then (if T then 1+X*3 else 5 fi) else (if F then 5 else 1+X*3 fi) fi" kr_g;;
