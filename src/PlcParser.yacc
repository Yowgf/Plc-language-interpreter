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
    | MATCH
    | WITH
    | DEFAULT

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
    | PIPE

    (* Unary Operators *)
    | NOT
    | MINUS
    | HEAD
    | TAIL
    | ISE
    | PRINT

    (* Binary Operators *)
    | AND
    | PLUS
    | MUL
    | DIV
    | NEQ
    | LT
    | GT
    | LEQ
    | GEQ
    | CAT

    (* Declaration stuff *)
    | VAR
    | FUN
    | REC
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
       | AppExpr of expr
       | Decl of expr

       (* Conditionals *)
       | Then_block of expr
       | Else_block of expr
       | Match_expr of (expr option * expr) list
       | Cond_expr of expr option

       (* Functions *)
       | Args of (plcType * string) list
       | Params of (plcType * string) list

       (* Operators *)
       | PrimU of expr
       | PrimB of expr

       (* Constants *)
       | Const of expr
       | Comps of expr list

       (* Types *)
       | Typed_var of plcType * string
       | Type of plcType
       | Atomic_type of plcType
       | Types of plcType list

(* Precedence *)

%right SEMICOLON GOESTO
%nonassoc IF
%nonassoc ELSE
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%right CAT
%left PLUS MINUS
%left MUL DIV
%nonassoc NOT HEAD TAIL ISE PRINT FUN
%left LBRAC

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
     | AppExpr (AppExpr)
     | IF Expr Then_block Else_block (If(Expr, Then_block, Else_block))
     | MATCH Expr WITH Match_expr (Match (Expr, Match_expr))
     | PrimU (PrimU)
     | PrimB (PrimB)
     | Expr LBRAC INT RBRAC (Item(INT, Expr))

Atomic_expr:
             Const (Const)
           | NAME (Var(NAME))
           | LCURL Program RCURL (Program)
           | LPAREN Expr RPAREN (Expr)
           | LPAREN Comps RPAREN (List(Comps))
           | ANON Args ARROW Expr END (makeAnon (Args, Expr))

AppExpr:
         Atomic_expr Atomic_expr %prec FUN (Call(Atomic_expr1, Atomic_expr2))
       | AppExpr Atomic_expr %prec FUN (Call(AppExpr, Atomic_expr))

Decl:
       VAR NAME EQ Expr SEMICOLON Program (Let(NAME, Expr, Program))
     | FUN NAME Args EQ Expr SEMICOLON Program (Let(NAME, makeAnon (Args, Expr), Program))
     | FUN REC NAME Args COLON Type EQ Expr SEMICOLON Program (makeFun(NAME, Args, Type, Expr, Program))


(* Conditionals *)

Then_block: THEN Expr (Expr)

Else_block: ELSE Expr (Expr)

Match_expr:
            END ([])
          | PIPE Expr GOESTO Expr Match_expr ((SOME(Expr1), Expr2)::Match_expr)
          | PIPE DEFAULT GOESTO Expr END ((NONE, Expr)::[])


(* Functions *)

Args:
      LPAREN RPAREN ([])
    | LPAREN Params RPAREN (Params)

Params: 
       Typed_var (Typed_var::[])
     | Typed_var COMMA Params (Typed_var::Params)


(* Operators *)

PrimU:
       NOT Expr (Prim1("!", Expr))
     | MINUS Expr (Prim1("-", Expr))
     | HEAD Expr (Prim1("hd", Expr))
     | TAIL Expr (Prim1("tl", Expr))
     | ISE Expr (Prim1("ise", Expr))
     | PRINT Expr (Prim1("print", Expr))

PrimB:
       Expr AND Expr (Prim2("&&", Expr1, Expr2))
     | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
     | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
     | Expr MUL Expr (Prim2("*", Expr1, Expr2))
     | Expr DIV Expr (Prim2("/", Expr1, Expr2))
     | Expr EQ Expr (Prim2("=", Expr1, Expr2))
     | Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
     | Expr LT Expr (Prim2("<", Expr1, Expr2))
     | Expr GT Expr (Prim2(">", Expr1, Expr2))
     | Expr LEQ Expr (Prim2("<=", Expr1, Expr2))
     | Expr GEQ Expr (Prim2(">=", Expr1, Expr2))
     | Expr CAT Expr (Prim2("::", Expr1, Expr2))
     | Expr SEMICOLON Expr (Prim2(";", Expr1, Expr2))

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
    | LPAREN Types RPAREN (ListT(Types))
    | LBRAC Type RBRAC (SeqT(Type))
    | Type GOESTO Type (FunT(Type1, Type2))

Atomic_type:
             INTT (IntT)
           | BOOLT (BoolT)
           | NILT (ListT([]))
           | LPAREN Type RPAREN (Type)

Types:
       Type COMMA Type (Type1 :: Type2 :: [])
     | Type COMMA Types (Type :: Types)
