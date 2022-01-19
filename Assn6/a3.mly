%{
    open A7
    exception Empty_Expression
%}

/* Tokens are defined below.  */
%token CALL DISPLAY RETURN EXIT COMMA LP RP EQ COLON EOF

%token <int> INT
%token <string> ID

%start parser
%type <A7.command> parser /* Returns expression */
%%

parser:
	CALL ID LP int_list RP                                  { Call($2,$4) }
  | DISPLAY                                               { Display }
  | RETURN                                                { Return }
  | EXIT                                                  { Exit }
  | ID COLON EQ INT                                       { Set ($1, Num $4)}
  | ID COLON EQ ID                                        { Set ($1, Str $4) }
  | EOF                                           				{ raise Empty_Expression }
;

int_list:
  INT COMMA int_list                                      {Num($1)::$3}
  | ID COMMA int_list                                     {Str($1)::$3}
  | ID                                                    {[Str($1)]}
  | INT                                                   {[Num($1)]}
