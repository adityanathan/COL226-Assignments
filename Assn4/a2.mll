(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file
*)
   {
    open A3
    exception InvalidToken of string

    let remove_sign s = if s.[0]<>'+' && s.[0]<>'-' then
                            INT (int_of_string s)
                       else if s.[0]='+' then
                                INT (int_of_string (String.sub s 1 ((String.length s)-1)))
                            else
                                INT (-1*int_of_string (String.sub s 1 ((String.length s)-1)))
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
   |    eof                     {EOF}
   |    whitespace              {read lexbuf}
   |    _                       {raise (InvalidToken ("Unexpected character: "^Lexing.lexeme lexbuf))}
