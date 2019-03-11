open A1

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let input_exptree = A3.main A2.read lexbuf in
            Printf.printf "Answer: "; print_int (A1.stackmc [] rho (compile input_exptree)); Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with Lexer.Eof ->
            exit 0
