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
    | LPAREN
    | RPAREN
    | LBRAC
    | RBRAC
    | LET
    | EQ
    | SEMICOLON
    | EOF

%nonterm Program of expr
       | Expr of expr
       | Atomic_expr of expr
       | Decl of expr
       | Const of expr
       | Type of plcType
       | Atomic_type of plcType

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

Decl: LET NAME EQ Const SEMICOLON Expr (Let(NAME, Const, Expr))

Atomic_expr:
       NAME (Var(NAME))
     | Const (Const)

Const:
       INT (ConI(INT))
     | TRUE (ConB(TRUE))
     | FALSE (ConB(FALSE))
     | LPAREN RPAREN (List([]))
     | LPAREN Type LBRAC RBRAC RPAREN (ESeq(Type))

Type: Atomic_type (Atomic_type)

Atomic_type:
             INTT (IntT)
           | BOOLT (BoolT)
           | NILT (ListT([]))

