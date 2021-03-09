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
    | EOF

%nonterm Const of expr
       | Type of plcType
       | Atomic_type of plcType
       | Atomic_expr of expr
       | Expr of expr

%eop EOF

%noshift EOF

%keyword

%start Expr

%verbose

%%

Expr:
       Atomic_expr(Atomic_expr)
     | LET NAME EQ Const(Let(NAME, Const, Const))

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

