%{
    open A5_type
    exception Tuple_value_not_an_integer
    exception Empty_Expression
    let extract_int x = match x with
    |  Integer (a) -> a
    | _     -> raise Tuple_value_not_an_integer
%}

/* Tokens are defined below.  */
%token COMMA TILDA LP RP IF THEN ELSE FI DELIMITER EOF EQ GT
LT ABS EXP DIV REM TIMES PLUS MINUS DISJ CONJ NOT PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF COLON TINT TBOOL TUNIT
ARROW CMP

%token <int> INT
%token <bool> BOOL
%token <string> ID

%start exp_parser
%type <A5_type.expr> exp_parser /* Returns expression */
%%

exp_parser:
	or_expr                                       { $1 }
  | EOF                                           				{ raise Empty_Expression }
;

or_expr:
  or_expr DISJ and_expr                           {Or ($1, $3)}
  | and_expr                                      {$1}
;

and_expr:
  and_expr CONJ not_expr                          {And ($1, $3)}
  | not_expr                                      {$1}
;

not_expr:
  /* NOT comparison_expr                             {Not ($2)} */
  | NOT not_expr                                  {Not ($2)}
  | comparison_expr                               {$1}
;

comparison_expr:
	CMP add_sub_expr																{Cmp($2)}
  | comparison_expr GT EQ add_sub_expr            {GreaterTE ($1, $4)}
  | comparison_expr LT EQ add_sub_expr            {LessTE ($1, $4)}
  | comparison_expr GT add_sub_expr               {GreaterT ($1, $3)}
  | comparison_expr LT add_sub_expr               {LessT ($1, $3)}
  | comparison_expr EQ add_sub_expr               {Equals ($1, $3)}
  | add_sub_expr                                  {$1}
;

add_sub_expr:
  add_sub_expr MINUS div_mult_rem_expr            {Sub ($1, $3)}
  | add_sub_expr PLUS div_mult_rem_expr           {Plus ($1, $3)}
  | div_mult_rem_expr                             {$1}
;

div_mult_rem_expr:
  div_mult_rem_expr TIMES abs_expr                {Mult ($1, $3)}
  | div_mult_rem_expr REM abs_expr                {Rem ($1, $3)}
  | div_mult_rem_expr DIV abs_expr                {Div ($1, $3)}
  | abs_expr                                      {$1}
;
/* exponent_expr:
    abs_expr EXP exponent_expr                      {Exp ($1, $3)}
    | abs_expr                                      {$1}
; */
abs_expr:
  | ABS abs_expr                                  {Abs ($2)}
  | unary_minus_expr                              {$1}
;

unary_minus_expr:
  | TILDA abs_expr                                {Negative($2)}
  | ifte_expr                                     {$1}
;

ifte_expr:
  IF or_expr THEN or_expr ELSE or_expr FI           {If_Then_Else($2,$4,$6)}
  | proj_expr                                       {$1}
;
proj_expr:
  PROJ LP constant COMMA constant RP ifte_expr     {Project ((extract_int $3, extract_int $5),$7)}
  | tuple_expr                                      {$1}
;
tuple_expr:
  LP or_expr COMMA tuple_sub1_expr RP       {Tuple (((List.length $4)+1), ($2::$4))}
  | function_call_expr                                      {$1}
;
tuple_sub1_expr:
  or_expr COMMA tuple_sub1_expr             {$1::$3}
  | or_expr                                 {[$1]}
;

function_call_expr:
	function_def_expr LP or_expr RP													{App($1,$3)}
	|	ID LP or_expr RP																			{App(V($1),$3)}
	| function_def_expr																			{$1}
	| paren_expr																							{$1}
;

function_def_expr:
BACKSLASH ID COLON type_expr DOT or_expr																											{Lambda($2,$6,$4)}
| LP BACKSLASH ID COLON type_expr DOT or_expr RP																							{Lambda($3,$7,$5)}
| BACKSLASH LP ID COMMA ID RP COLON LP type_expr COMMA type_expr RP DOT or_expr								{RecursiveLambda($3,$5,$14,$9,$11)}
| LP BACKSLASH LP ID COMMA ID RP COLON LP type_expr COMMA type_expr RP DOT or_expr RP				{RecursiveLambda($4,$6,$15,$10,$12)}
;

paren_expr:
LP or_expr RP                                     {InParen($2)}
| constant                                        {$1}
	;

constant:
    ID                                              {V ($1)}
    | INT                                           {Integer ($1)}
    | BOOL                                          {Bool ($1)}
;

type_expr:
	TINT																							{ Tint }
	|	TBOOL																						{ Tbool }
	|	TUNIT																						{	Tunit }
	|	type_tuple_expr																	{$1}
	| type_func_expr																	{$1}
;

type_tuple_expr:
  LP type_expr TIMES type_tuple_sub1_expr RP       {Ttuple ($2::$4)}
;
type_tuple_sub1_expr:
  type_expr TIMES type_tuple_sub1_expr             {$1::$3}
  | type_expr                                 			{[$1]}
;

type_func_expr:
	type_expr ARROW type_expr													{Tfunc($1,$3)}
;
