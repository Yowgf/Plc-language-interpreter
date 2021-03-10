%%

%name PlcParser

%pos int

%term INT of int
    | TRUE of bool
    | FALSE of bool
    | INTT of plcType
    | BOOLT of plcType
    | NILT of plcType
    | NAME of string
    | IF
    | THEN
    | ELSE
    | LPAREN
    | RPAREN
    | LBRAC
    | RBRAC
    | LET
    | EQ
    | FUN
    | SEMICOLON
    | COLON
    | COMMA
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
       | Params of (plcType * string) list
       | Args of (plcType * string) list

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

Then_block: THEN Expr (Expr)

Else_block: ELSE Expr (Expr)

Function_block: Expr (Expr)

Decl: 
       LET NAME EQ Const SEMICOLON Expr (Let(NAME, Const, Expr))
     | FUN NAME Args EQ Function_block SEMICOLON Expr (Let(NAME, makeAnon (Args, Function_block), Expr))

Atomic_expr:
       NAME (Var(NAME))
     | Const (Const)

Const:
       INT (ConI(INT))
     | TRUE (ConB(TRUE))
     | FALSE (ConB(FALSE))
     | LPAREN RPAREN (List([]))
     | LPAREN Type LBRAC RBRAC RPAREN (ESeq(Type))

Args: LPAREN Params RPAREN (Params)

Params: 
       Typed_var (Typed_var::[])
     | Typed_var COMMA Params (Typed_var::Params)

Typed_var: Type NAME (Type, NAME)

Type: Atomic_type (Atomic_type)

Atomic_type:
             INTT (IntT)
           | BOOLT (BoolT)
           | NILT (ListT([]))

