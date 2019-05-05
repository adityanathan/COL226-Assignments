#directory "_build";;
#load "a7.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A3;;
open A2;;
open A7;;

let bef = [new_pointer ((List.hd procedure_database),[("a",Undefined);("b",Undefined);("c",Undefined)])];;
let call_stack : (frame_pointer list) = bef;;
let display_reg : (frame_pointer list) = bef@[Null;Null;Null];;
let dump : (frame_pointer list) = [];;

let rec simulation call_stack dump display_reg=
  Printf.printf "______________________ \n";
  Printf.printf "Enter your command:\n";
  let s = read_line () in
  let command = A3.parser A2.read (Lexing.from_string s) in
  match command with
    Call (a,b) -> (match call_update a b call_stack display_reg dump with (st,du,disp) -> simulation st du disp)
  | Display -> print_stack call_stack display_reg; print_disp (List.rev display_reg); simulation call_stack dump display_reg
  | Set(var_name, value) -> let current_pr = !^(List.hd call_stack) in
    (match current_pr with
       (Proc(a,b,c,d,e,f),tabl) ->let _ = ((List.hd call_stack) ^:= (Proc(a,b,c,d,e,f),replace_var_from_table var_name (substitute_variable value tabl) tabl c display_reg)) in simulation call_stack dump display_reg)
  | Return -> (match call_stack,dump,display_reg with
        hd::tl, top::bot, disp -> let current_pr = !^(List.hd call_stack) in (match current_pr with
                                      |(Proc(a,b,c,d,e,f),_) -> simulation tl bot (replace_elem top display_reg b)))
  | Exit -> Printf.printf "Exited Simulation.\n"

let _ = simulation call_stack dump display_reg
