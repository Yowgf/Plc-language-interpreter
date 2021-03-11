%% (* End User Declarations section, start Declarations section *)

%name PlcParser

%pos int

%term
    (* Constants *)
      INT of int
    | TRUE of bool
    | FALSE of bool
    | INTT of plcType
    | BOOLT of plcType
    | NILT of plcType

    (* Conditionals *)
    | IF
    | THEN
    | ELSE

    (* Grouping symbols *)
    | LPAREN
    | RPAREN
    | LBRAC
    | RBRAC
    | LCURL
    | RCURL

    (* Separating symbols *)
    | EQ
    | SEMICOLON
    | COLON
    | COMMA
    | ARROW
    | GOESTO

    (* Unary Operators *)
    | NOT
    | NEG
    | HEAD
    | TAIL
    | ISE
    | PRINT

    (* Declaration stuff *)
    | VAR
    | FUN
    | ANON
    | END

    (* Identifier *)
    | NAME of string
    
    | EOF

%nonterm
         Start of expr
       | Program of expr
       
       (* Expr and Decl *)
       | Expr of expr
       | Atomic_expr of expr
       | Decl of expr

       (* Conditionals *)
       | Then_block of expr
       | Else_block of expr

       (* Functions *)
       | Function_block of expr
       | Args of (plcType * string) list
       | Params of (plcType * string) list

       (* Operators *)
       | PrimU of expr

       (* Constants *)
       | Const of expr
       | Comps of expr list

       (* Types *)
       | Typed_var of plcType * string
       | Type of plcType
       | Atomic_type of plcType

(* Precedence *)
%right GOESTO

(* Special properties of End Of File token *)
%eop EOF
%noshift EOF

(* Make sure to generate .desc file *)
%verbose

%start Start

%% (* End Declarations section, start rules section *)

(* Necessary so that Program may be reduced to *)
Start: Program (Program)

Program:
         Expr (Expr)
       | Decl (Decl)


(* Expr and Decl *)

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


(* Conditionals *)

Then_block: THEN Expr (Expr)

Else_block: ELSE Expr (Expr)


(* Functions *)

Args:
      LPAREN RPAREN ([])
    | LPAREN Params RPAREN (Params)

Function_block: Expr (Expr)

Params: 
       Typed_var (Typed_var::[])
     | Typed_var COMMA Params (Typed_var::Params)


(* Operators *)

PrimU:
       NOT Expr (Prim1("!", Expr))
     | NEG Expr (Prim1("-", Expr))
     | HEAD Expr (Prim1("hd", Expr))
     | TAIL Expr (Prim1("tl", Expr))
     | ISE Expr (Prim1("ise", Expr))
     | PRINT Expr (Prim1("print", Expr))


(* Constants *)

Const:
       INT (ConI(INT))
     | TRUE (ConB(TRUE))
     | FALSE (ConB(FALSE))
     | LPAREN RPAREN (List([]))
     | LPAREN Type LBRAC RBRAC RPAREN (ESeq(Type))

Comps:
       Expr COMMA Expr (Expr1 :: Expr2 :: [])
     | Expr COMMA Comps (Expr :: Comps)


(* Types *)

Typed_var: Type NAME (Type, NAME)

Type:
      Atomic_type (Atomic_type)
    | Type GOESTO Type (FunT(Type1, Type2))

Atomic_type:
             INTT (IntT)
           | BOOLT (BoolT)
           | NILT (ListT([]))
           | LPAREN Type RPAREN (Type)
