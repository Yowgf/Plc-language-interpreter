%%

%name PlcParser

%pos int

%term INT of int
    | TRUE of bool
    | FALSE of bool
    | INTT of plcType
    | BOOLT of plcType
    | NILT of plcType
    | IF
    | THEN
    | ELSE
    | LPAREN
    | RPAREN
    | LBRAC
    | RBRAC
    | EQ
    | SEMICOLON
    | COLON
    | COMMA
    | ARROW
    | LET
    | FUN
    | ANON
    | END
    | NAME of string
    | EOF

%nonterm Program of expr
       | Expr of expr
       | Atomic_expr of expr
       | Decl of expr
       | Const of expr
       | Typed_var of plcType * string
       | Type of plcType
       | Atomic_type of plcType
       | Then_block of expr
       | Else_block of expr
       | Function_block of expr
       | Args of (plcType * string) list
       | Params of (plcType * string) list

%eop EOF

%noshift EOF

%keyword

%start Program

%verbose

%%

Program: Expr (Expr)

Expr:
       Atomic_expr (Atomic_expr)
     | Decl (Decl)
     | IF Expr Then_block Else_block (If(Expr, Then_block, Else_block))

Atomic_expr:
       NAME (Var(NAME))
     | Const (Const)
     | ANON Args ARROW Function_block END (makeAnon (Args, Function_block))

Decl: 
       LET NAME EQ Const SEMICOLON Expr (Let(NAME, Const, Expr))
     | FUN NAME Args EQ Function_block SEMICOLON Expr (Let(NAME, makeAnon (Args, Function_block), Expr))

Const:
       INT (ConI(INT))
     | TRUE (ConB(TRUE))
     | FALSE (ConB(FALSE))
     | LPAREN RPAREN (List([]))
     | LPAREN Type LBRAC RBRAC RPAREN (ESeq(Type))

Typed_var: Type NAME (Type, NAME)

Type: Atomic_type (Atomic_type)

Atomic_type:
             INTT (IntT)
           | BOOLT (BoolT)
           | NILT (ListT([]))

Then_block: THEN Expr (Expr)

Else_block: ELSE Expr (Expr)

Function_block: Expr (Expr)

Args: LPAREN Params RPAREN (Params)

Params: 
       Typed_var (Typed_var::[])
     | Typed_var COMMA Params (Typed_var::Params)

