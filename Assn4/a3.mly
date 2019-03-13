%{
    open A1
    exception Tuple_value_not_an_integer
    exception Empty_Expression
    let extract_int x = match x with
      N (a) -> a
    | _     -> raise Tuple_value_not_an_integer
%}

/* Tokens are defined below.  */
%token COMMA TILDA LP RP IF THEN ELSE FI DELIMITER EOF EQ GT LT ABS EXP DIV REM TIMES PLUS MINUS DISJ CONJ NOT PROJ
%token <int> INT
%token <string> ID
%token <bool> BOOL

%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
    or_expr DELIMITER                               { $1 }
    | or_expr                                       { $1 }
    | EOF                                           { raise Empty_Expression }
;

or_expr:
    or_expr DISJ and_expr                           {Disjunction ($1, $3)}
    | and_expr                                      {$1}
;

and_expr:
    and_expr CONJ not_expr                          {Conjunction ($1, $3)}
    | not_expr                                      {$1}
;

not_expr:
    NOT comparison_expr                             {Not ($2)}
    | comparison_expr                               {$1}
;

comparison_expr:
    comparison_expr GT EQ add_sub_expr              {GreaterTE ($1, $4)}
    | comparison_expr LT EQ add_sub_expr            {LessTE ($1, $4)}
    | comparison_expr GT add_sub_expr               {GreaterT ($1, $3)}
    | comparison_expr LT add_sub_expr               {LessT ($1, $3)}
    | comparison_expr EQ add_sub_expr               {Equals ($1, $3)}
    | add_sub_expr                                  {$1}
;

add_sub_expr:
    add_sub_expr MINUS div_mult_rem_expr            {Sub ($1, $3)}
    | add_sub_expr PLUS div_mult_rem_expr           {Add ($1, $3)}
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
    ABS unary_minus_expr                            {Abs ($2)}
    | unary_minus_expr                              {$1}
;

unary_minus_expr:
    TILDA ifte_expr                                 {Negative($2)}
    | ifte_expr                                     {$1}
;

ifte_expr:
  IF or_expr THEN or_expr ELSE or_expr FI           {IfThenElse($2,$4,$6)}
  | proj_expr                                       {$1}
;
proj_expr:
  PROJ LP constant COMMA constant RP tuple_expr     {Project ((extract_int $3, extract_int $5),$7)}
  | tuple_expr                                      {$1}
;
tuple_expr:
  LP tuple_sub2_expr COMMA tuple_sub1_expr RP       {Tuple (((List.length $4)+1), ($2::$4))}
  | paren_expr                                      {$1}
;
tuple_sub1_expr:
  tuple_sub2_expr COMMA tuple_sub1_expr             {$1::$3}
  | tuple_sub2_expr                                 {[$1]}
;
tuple_sub2_expr:
  or_expr                                           {$1}
  | paren_expr                                      {$1}
;
paren_expr:
  LP or_expr RP                                     {InParen($2)}
  | constant                                        {$1}
;

constant:
    ID                                              {Var ($1)}
    | INT                                           {N ($1)}
    | BOOL                                          {B ($1)}
;
