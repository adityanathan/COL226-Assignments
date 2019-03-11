%{
    open A1
%}

/* Tokens are defined below.  */
%token TRUE FALSE LP RP IF THEN ELSE DEF DELIMITER EOF
%token <int> INT
%token <string> ID



%left MINUS
%left PLUS
%left MUL
%left DIV MOD
%right EXP
%nonassoc ABS
%nonassoc EQ GTA LTA GEQ LEQ
%left AND
%left OR
%left NOT

%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
    expr DELIMITER              { $1 }
    |EOF                            { NULL }

expr:
    INT             {N $1}
    TRUE            {B true}
    FALSE           {B false}
    NOT expr        {Not $2}
    expr OR expr    {Disjunction ($1,$3)}
    expr AND expr   {Conjunction ($1,$3)}
    expr EQ expr    {Equals ($1,$3)}
    expr GTA expr   {GreaterT ($1,$3)}
    expr LTA expr   {LessT ($1,$3)}
    expr GEQ expr   {GreaterTE ($1,$3)}
    expr LEQ expr   {LessTE ($1,$3)}
    ABS expr        {Abs ($2)}
    expr EXP expr   {Exp ($1,$3)}
    expr DIV expr   {Div ($1,$3)}
    expr MOD expr   {Rem ($1,$3)}
    expr MUL expr   {Mult ($1,$3)}
    expr PLUS expr  {Plus ($1,$3)}
    expr MINUS expr {Minus ($1,$3)}



;
