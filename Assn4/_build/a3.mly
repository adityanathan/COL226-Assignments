%{
    open A1
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
    or_expr DELIMITER                         { $1 }
    | or_expr                                 { $1 }
    | EOF                                       { Done }
;

or_expr:
    or_expr DISJ and_expr                         {Disjunction ($1, $3)}
    | and_expr                                  {$1}
;

and_expr:
    and_expr CONJ not_expr                        {Conjunction ($1, $3)}
    | not_expr                                   {$1}
;

not_expr:
    NOT comparison_expr                              {Not ($2)}
    | comparison_expr                                {$1}
;

comparison_expr:
    comparison_expr GT EQ sub_expr             {GreaterTE ($1, $4)}
    | comparison_expr LT EQ sub_expr           {LessTE ($1, $4)}
    | comparison_expr GT sub_expr              {GreaterT ($1, $3)}
    | comparison_expr LT sub_expr              {LessT ($1, $3)}
    | comparison_expr EQ sub_expr               {Equals ($1, $3)}
    | sub_expr                                  {$1}
;

sub_expr:
    sub_expr MINUS add_expr                     {Sub ($1, $3)}
    | add_expr                                  {$1}
;

add_expr:
    add_expr PLUS mult_expr                     {Add ($1, $3)}
    | mult_expr                                 {$1}
;
mult_expr:
    mult_expr TIMES mod_expr                      {Mult ($1, $3)}
    | mod_expr                                  {$1}
;
mod_expr:
    mod_expr REM div_expr                       {Rem ($1, $3)}
    | div_expr                                  {$1}
;
div_expr:
    div_expr DIV exponent_expr                  {Div ($1, $3)}
    | exponent_expr                             {$1}
;
exponent_expr:
    abs_expr EXP exponent_expr                  {Exp ($1, $3)}
    | abs_expr                                  {$1}
;
abs_expr:
    ABS unary_minus_expr                         {Abs ($2)}
    | unary_minus_expr                           {$1}
;

unary_minus_expr:
    TILDA paren_expr                              {Negative($2)}
    | paren_expr                                  {$1}

paren_expr:
    LP or_expr RP                         {InParen($2)}
    | ifte_expr                                  {$1}
;

ifte_expr:
  IF or_expr THEN or_expr ELSE or_expr FI  {IfThenElse($2,$4,$6)}
  | constant                                  {$1}
;

constant:
    ID                                          {Var ($1)}
    | INT                                       {N ($1)}
    | BOOL                                      {B ($1)}
;
