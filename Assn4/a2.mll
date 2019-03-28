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
   let integer_constant ='0'|nz_digit digit*
   let identifiers = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
   let whitespace = ' '|'\n'|'\r'|'\t'
   (* let trailing_zero_error = ['+' '-']? ('0')(('0')+)|['+' '-']? nz_digit digit*
   let beginning_char_error = ['T'|'F'] identifiers *)


   rule read = parse
   |    ','                     {COMMA}
   |    '~'                     {TILDA}
   |    "abs"                   {ABS}
   |    '+'                     {PLUS}
   |    '-'                     {MINUS}
   |    integer_constant as x   {remove_sign x}
   |    '*'                     {TIMES}
   |    "div"                   {DIV}
   |    "mod"                   {REM}
   |    '^'                     {EXP}
   |    '('                     {LP}
   |    ')'                     {RP}
   |    'T'                     {BOOL (true)}
   |    'F'                     {BOOL (false)}
   |    "not"                   {NOT}
   |    "/\\"                   {CONJ}
   |    "\\/"                   {DISJ}
   |    '='                     {EQ}
   |    '<'                     {LT}
   |    '>'                     {GT}
   |    "if"                    {IF}
   |    "then"                  {THEN}
   |    "else"                  {ELSE}
   |    "fi"                    {FI}
   |    "proj"                  {PROJ}
   |    identifiers as x        {ID (x)}
   |    ';'                     {DELIMITER}
   |    eof                     {EOF}
   |    whitespace              {read lexbuf}
   |    _                       {raise (InvalidToken ("Unexpected character: "^Lexing.lexeme lexbuf))}
