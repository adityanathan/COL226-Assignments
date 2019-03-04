{
type token =
   INT of int          (* integer constant, positive or negative w/o leading zeros *)
|  TRUE                (* boolean constant "T" *)
|  FALSE               (* boolean constant "F" *)
|  ABS                 (* unary operator, "abs" *)
|  PLUS                (* arithmetic plus, "+" *)
|  MINUS               (* arithmetic minus, "-" *)
|  MUL                 (* arithmetic multiply, "*" *)
|  DIV                 (* integer div, "div" *)
|  MOD                 (* remainder, "mod" *)
|  EXP                 (* exponentiation, "^" *)
|  LP                  (* left paren, "(" *)
|  RP                  (* right paren, ")" *)
|  NOT                 (* boolean NOT, "not" *)
|  AND                 (* boolean AND, "/\ " *)
|  OR                  (* boolean OR, "\/" *)
|  EQ                  (* equal to, "=" *)
|  GTA                 (* greater than, ">" *)
|  LTA                 (* less than, "<" *)
|  GEQ                 (* greater than/equal to, ">=" *)
|  LEQ                 (* less than/equal to, "<=" *)
|  IF                  (* keyword "if" *)
|  THEN                (* keyword "then" *)
|  ELSE                (* keyword "else" *)
|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
|  DEF                 (* definition construct, "def" *)
|  DELIMITER;;         (* delimiter, ";" *)
exception InvalidToken of string

let remove_sign s = if s.[0]<>'+' && s.[0]<>'-' then
                        INT (int_of_string s)
                    else
                        INT (int_of_string (String.sub s 1 ((String.length s)-1)))
(* remove_sign has been defined to help remove the + or - sign at the beginning of a string. *)
(* It has been assumed that given string will have only one sign as reg exp takes only one sign *)
}

let nz_digit = ['1'-'9']
let digit = ['0'-'9']
let integer_constant = ['+' '-']? '0'|['+' '-']? nz_digit digit*
let identifiers = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let whitespace = ' '|'\n'|'\r'|'\t'
(* let trailing_zero_error = ['+' '-']? ('0')(('0')+)|['+' '-']? nz_digit digit*
let beginning_char_error = ['T'|'F'] identifiers *)


rule read = parse

|    "abs"                   {ABS::read lexbuf}
|    '+'                     {PLUS::read lexbuf}
|    '-'                     {MINUS::read lexbuf}
|    integer_constant as x   {(remove_sign x)::read lexbuf }
|    '*'                     {MUL::read lexbuf}
|    "div"                   {DIV::read lexbuf}
|    "mod"                   {MOD::read lexbuf}
|    '^'                     {EXP::read lexbuf}
|    '('                     {LP::read lexbuf}
|    ')'                     {RP::read lexbuf}
|    'T'                     {TRUE::read lexbuf}
|    'F'                     {FALSE::read lexbuf}
|    "not"                   {NOT::read lexbuf}
|    "/\\"                   {AND::read lexbuf}
|    "\\/"                   {OR::read lexbuf}
|    '='                     {EQ::read lexbuf}
|    '<'                     {LTA::read lexbuf}
|    '>'                     {GTA::read lexbuf}
|    ">="                    {GEQ::read lexbuf}
|    "<="                    {LEQ::read lexbuf}
|    "if"                    {IF::read lexbuf}
|    "then"                  {THEN::read lexbuf}
|    "else"                  {ELSE::read lexbuf}
|    identifiers as x        {ID (x):: read lexbuf}
|    "def"                   {DEF::read lexbuf}
|    ';'                     {DELIMITER::read lexbuf}
|    eof                     {[]}
|    whitespace              {read lexbuf}
|    _                       {raise (InvalidToken ("Unexpected character: "^Lexing.lexeme lexbuf))}

{
    let scanner s = read (Lexing.from_string s)
}
(* 3+5 will be read as INT(3), INT(5) and 3+ 5 will be read as INT(3), PLUS, INT(5) *)
(* Whitespace has not been enforced. This will be taken care of in the parsing stage *)
(* Test cases:
Examples
        "let i=-10 if i>103 then exit else (abs i)++;;"
        "let i=10 if i>=0 then fact*=i else i--;;"
        ""
        "let i=10000 if i<100 then exit else i div= 2;;"
        "if F then T else if T else F;;"
        "let a = 5 mod -3;;"
        "def a = bleh of int | string;;"
        "let a = +5 mod -3;;
         def a = bleh of int | string;; "
        "a \/ b /\\ c"
        "+5"
        "+ 5"
        "52+5"
        "52 + 5"
        "4not3absT"
        "00123"
        "+0","-0","0"
Counter-examples:
        "not Not"
        "3#5"
        "GGGah"
        "Ax123"
        "AND"
        "?"
        "[]]"
        ""
        "\"\""
            *)
