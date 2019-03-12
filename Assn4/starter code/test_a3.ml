#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";; (* Load the a0 bytecode *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
  | _ -> raise Not_implemented
;;

let rec print_value tr = match tr with
  NumVal a -> string_of_int a
  | BoolVal a -> string_of_bool a
  | _ -> raise Not_implemented
;;

(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;


(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

(* Parser accepts the expression as string and binding as hash map with variable to values (integer, boolean, tuple) *)
let parser s rho =
  let result = A3.main A2.read (Lexing.from_string s) in
    (* Return the three versions as abstract syntax tree, value, compiled opcode*)
    (result, (A1.eval result rho), (A1.stackmc [] (binding rho) (A1.compile result)))
;;

(* Input is given as string and output is a value *)
let rho s = match s with 
   "X" -> NumVal 5
|  "Y" -> BoolVal true
|  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1]);;

let _ = (parser "5" rho);;
