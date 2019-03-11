#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";; (* Load the a0 bytecode *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;

(* Helper function to print the tree *)
let rec print_tree tr = match tr with
  Done -> "\n"
  | N a -> "INT " ^ (string_of_int a)
;;


(* Parser accepts the expression as string and binding as hash map with variabl to values (integer, boolean, tuple) *)
let parser s binding =
  let result = A3.main A2.readRows (Lexing.from_string s) in
  (* Print out for debugging *)
    Printf.printf "Tree: ";
    print_string ( print_tree result );
    Printf.printf "\nAnswer: ";
    print_endline (string_of_int (A1.eval result));
    flush stdout;
  (* Return the three versions as abstract syntax tree, value, compiled opcode*)
;;


(* Input is given as string *)
module binding = Map.Make(String)
let variable_set = binding.empty;;
let _ = parser "5;" binding;;
