open Expression

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = Parser.main Lexer.token lexbuf in
            Expression.print_tree result 0;Printf.printf "Answer: "; print_int (Expression.eval_tree result); Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with Lexer.Eof ->
            exit 0
