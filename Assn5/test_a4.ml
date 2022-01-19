#directory "_build";; (* Consider this folder when looking for files *)
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
#load "a5_krivine.cmo";;
#load "a5_secd.cmo";;
#load "a5_type.cmo"
open A2;;
open A3;;
open A4;;
open A5_krivine;;
open A5_secd;;
open A5_type;;

exception Not_implemented;;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;

let kr_g = [("X", Integer 2)];;
let s_g = [("X", IntVal 2)];;
let type_g = [("X", Tint)];;

let input_expr e= A3.exp_parser A2.read (Lexing.from_string e);;
let kmc e = krivine_machine (input_expr e) kr_g type_g;;
let smc e = secd_machine (input_expr e) s_g type_g;;

kmc "1+2*3";;
smc "1+2*3";;

kmc "if T then (if T then 1+X*3 else 5 fi) else (if F then 5 else 1+X*3 fi) fi";;
smc "if T then (if T then 1+X*3 else 5 fi) else (if F then 5 else 1+X*3 fi) fi";;

kmc "if T then (if T then 1+X*3 else F fi) else (if F then 5 else 1+X*3 fi) fi";;
smc "if T then (if T then 1+X*3 else F fi) else (if F then 5 else 1+X*3 fi) fi";;

kmc "\\W:Tint.(\\Y:Tint.(Y+2)(W))(2)";;
smc "\\W:Tint.(\\Y:Tint.(Y+2)(W))(2)";;

kmc "1+T";;
smc "1+T";;

kmc "if cmp 7 then \\X:Tint.(3+X)(31) else 0 fi";;
smc "if cmp 7 then \\X:Tint.(3+X)(31) else 0 fi";;

kmc "\\X:Tint.(\\Y:Tint.((\\X:Tint.X)(4) + 4)(3))(2)";;
smc "\\X:Tint.(\\Y:Tint.((\\X:Tint.X)(4) + 4)(3))(2)";;

kmc "(\\(N, X):(Tint,Tint).if cmp X then X*(N(X+(~1))) else 1 fi)(3)";;
smc "(\\(N, X):(Tint,Tint).if cmp X then X*(N(X+(~1))) else 1 fi)(3)";;
