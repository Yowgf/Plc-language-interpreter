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
    | LCURL
    | RCURL
    | EQ
    | SEMICOLON
    | COLON
    | COMMA
    | ARROW
    | GOESTO
    | NOT
    | NEG
    | HEAD
    | TAIL
    | ISE
    | PRINT
    | VAR
    | FUN
    | ANON
    | END
    | NAME of string
    | EOF

%nonterm Start of expr
       | Program of expr
       | Expr of expr
       | Atomic_expr of expr
       | Decl of expr
       | Const of expr
       | Typed_var of plcType * string
       | Comps of expr list
       | PrimU of expr
       | Type of plcType
       | Atomic_type of plcType
       | Then_block of expr
       | Else_block of expr
       | Function_block of expr
       | Args of (plcType * string) list
       | Params of (plcType * string) list

%eop EOF

%noshift EOF

%right GOESTO

%start Start

%verbose

%%

Start: Program (Program)

Program:
         Expr (Expr)
       | Decl (Decl)

Expr:
       Atomic_expr (Atomic_expr)
     | IF Expr Then_block Else_block (If(Expr, Then_block, Else_block))
     | PrimU (PrimU)

Atomic_expr:
             Const (Const)
           | NAME (Var(NAME))
           | LCURL Program RCURL (Program)
           | LPAREN Expr RPAREN (Expr)
           | LPAREN Comps RPAREN (List(Comps))
           | ANON Args ARROW Function_block END (makeAnon (Args, Function_block))

Decl:
       VAR NAME EQ Expr SEMICOLON Program (Let(NAME, Expr, Program))
     | FUN NAME Args EQ Function_block SEMICOLON Program (Let(NAME, makeAnon (Args, Function_block), Program))

Then_block: THEN Expr (Expr)

Else_block: ELSE Expr (Expr)

Function_block: Expr (Expr)

Args:
      LPAREN RPAREN ([])
    | LPAREN Params RPAREN (Params)

Params: 
       Typed_var (Typed_var::[])
     | Typed_var COMMA Params (Typed_var::Params)

Typed_var: Type NAME (Type, NAME)

PrimU:
       NOT Expr (Prim1("!", Expr))
     | NEG Expr (Prim1("-", Expr))
     | HEAD Expr (Prim1("hd", Expr))
     | TAIL Expr (Prim1("tl", Expr))
     | ISE Expr (Prim1("ise", Expr))
     | PRINT Expr (Prim1("print", Expr))

Type:
      Atomic_type (Atomic_type)
    | Type GOESTO Type (FunT(Type1, Type2))

Atomic_type:
             INTT (IntT)
           | BOOLT (BoolT)
           | NILT (ListT([]))
           | LPAREN Type RPAREN (Type)

Comps:
       Expr COMMA Expr (Expr1 :: Expr2 :: [])
     | Expr COMMA Comps (Expr :: Comps)

Const:
       INT (ConI(INT))
     | TRUE (ConB(TRUE))
     | FALSE (ConB(FALSE))
     | LPAREN RPAREN (List([]))
     | LPAREN Type LBRAC RBRAC RPAREN (ESeq(Type))
