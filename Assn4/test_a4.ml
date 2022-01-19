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
let rec print_def df = match df with
  Simple(l,r) -> "def " ^ l ^ " = " ^ (print_tree r)
  | _ -> raise Not_implemented
;;


(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s rho = A3.exp_parser A2.read (Lexing.from_string s) ;;
let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;;

(* Input is given as string and output is a value *)
let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;;

(* Sample parsing *)
(* print_endline ( print_tree (exp_parser "5" rho));;
print_endline ( print_def (def_parser "def a=5" rho));;

(* Sample test case *)
let e = (exp_parser "\\X.Y" rho);;
let t = Tfunc (Tint, Tbool);;

(* Type assumptions as a list of tuples of the form (variable name, type) *)
let g = [("X", Tint); ("Y", Tbool); ("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];;
let d = (def_parser "def U = X ; def V = Y" rho);;
let g_dash = [("U", Tint); ("V", Tbool)];; *)

(* assert(hastype g e t);;
assert(yields g d g_dash);; *)

let g = [("Y", Tbool)];;

(* Explicit type declaration for function abstraction *)
let e = exp_parser "\\X:Tint.X" rho;;
let t = Tfunc(Tint, Tint);;
hastype g e t == true;; (* should return true *)

(* Types of both if-branches should match. *)
let e = exp_parser "\\X:Tint.(if Y then X else T fi);;" rho;;
let t = Tfunc(Tint, Tbool);;
hastype g e t == false;; (* should be false, as the two branches do not have same type *)

(* Explicit type declaration for definition. *)
let e = exp_parser "let def X:Tbool = Y in X end" rho;;
let t = Tbool;;
hastype g e t == true;; (* should return true *)

(* Incorrect type declaration for definition. *)
let e = exp_parser "let def X:Tint = Y in X end" rho;;
let t = Tbool;;
hastype g e t == false;; (* should return false, as the claimed type of X is not possible w.r.t. type assumptions g *)

(* Sequential composition of definitions. *)
let e = exp_parser "let def X:Tint = 3; def Z:Tint = 2*X in Z end" rho;;
let t = Tint;;
hastype g e t;; (* should return true *)

(* Parallel composition of definitions. *)
let e = exp_parser "let def X:Tint = 3 || def Z:Tint = 2*X in Z end" rho;;
let t = Tint;;
hastype g e t == false;; (* should return false, as in second branch X is undefined *)
(* assume that the set of variables in the two branches for parallel composition will be disjoint. --*)

(* Nested let bindings. *)
let e = exp_parser "let def X:Tint = 3 in (let def X:Tbool = T in X end) end" rho;;
let t = Tbool;;
hastype g e t == true;; (* should return true *)

(* More complicated type declarations *)
let e = exp_parser "let def Foo:Tint -> (Tint * Tbool) = \\X:Tint.(X,Y) in Foo(5) end" rho;;
let t = Ttuple([Tint; Tbool]);;
hastype g e t == true;; (* should return true *)

(* To support the following kind of type-checks, you could introduce a new constructor in the "type" datatype, say "TypeVar of string", which represents a named type variable and which could take any possible type. *)
let e = exp_parser "proj(1,2) if T then (3,T) else (4,F) fi" rho;;
let t = Tint;;
hastype g e t == true;; (* should return true *)

(* You may have to implement some equation solving to handle such cases. *)
let e = exp_parser "proj(1,2) if X then (3,X) else (4,4) fi" rho;;
let t = Tint;;
hastype g e t == false;; (* should return false since the ifte expression forces one branch to be of type Ttuple(Tint, Tbool) and the other branch to be of type Ttuple(Tint, Tint *)
