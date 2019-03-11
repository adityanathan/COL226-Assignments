{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a3.ml)
*)
rule read = parse
   eof                { EOF }
   | ['0'-'9']+ as n  { INT (int_of_string n) }
   | _                { raise Not_implemented }
