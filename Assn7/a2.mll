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
   let identifiers = ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*
   let whitespace = ' '|'\n'|'\r'|'\t'

   rule read = parse
   |    "call"                  {CALL}
   |    "display"               {DISPLAY}
   |    "return"                {RETURN}
   |    "exit"                  {EXIT}
   |    ','                     {COMMA}
   |    integer_constant as x   {remove_sign x}
   |    '('                     {LP}
   |    ')'                     {RP}
   |    '='                     {EQ}
	 |		':'											{COLON}
   |    identifiers as x        {ID (x)}
   |    eof                     {EOF}
   |    whitespace              {read lexbuf}
   |    _                       {raise (InvalidToken ("Unexpected character: "^Lexing.lexeme lexbuf))}
