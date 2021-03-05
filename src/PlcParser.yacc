%%

%name PlcParser

%pos int

%term INT of int
    | TRUE of bool
    | FALSE of bool
    | INTT of plcType
    | BOOLT of plcType
    | NILT of plcType
    | LPAREN
    | RPAREN
    | LBRAC
    | RBRAC
    | EOF

%nonterm Const of expr
       | Type of plcType
       | Atomic_type of plcType

%eop EOF

%noshift EOF

%keyword

%start Const

%verbose

%%

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
