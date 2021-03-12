(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos) token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()

(* Tokenizers *)
fun integer(str, lexPos) =
  case Int.fromString(str) of
       NONE => raise Fail("Sequence of digits not recognized as " ^
                          "integer--" ^ str)
     | SOME(n) => INT(n, lexPos, lexPos)

%%

%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

ws  = [\ \t\n]+;
nat = [0-9]+;
name = [a-zA-Z_][a-zA-Z0-9_]*;
comment = "(*"(.|\n)*?"*)";

%%

{ws} => (lex());
{comment} => (lex());

{nat} => (integer(yytext, yypos));
"true" => (TRUE(true, yypos, yypos));
"false" => (FALSE(false, yypos, yypos));
"Int" => (INTT(IntT, yypos, yypos));
"Bool" => (BOOLT(BoolT, yypos, yypos));
"Nil" => (NILT(ListT([]), yypos, yypos));

"if" => (IF(yypos, yypos));
"then" => (THEN(yypos, yypos));
"else" => (ELSE(yypos, yypos));
"match" => (MATCH(yypos, yypos));
"with" => (WITH(yypos, yypos));
"_" => (DEFAULT(yypos, yypos));

"(" => (LPAREN(yypos, yypos));
")" => (RPAREN(yypos, yypos));
"[" => (LBRAC(yypos, yypos));
"]" => (RBRAC(yypos, yypos));
"{" => (LCURL(yypos, yypos));
"}" => (RCURL(yypos, yypos));

"=" => (EQ(yypos, yypos));
";" => (SEMICOLON(yypos, yypos));
":" => (COLON(yypos, yypos));
"," => (COMMA(yypos, yypos));
"=>" => (ARROW(yypos, yypos));
"->" => (GOESTO(yypos, yypos));
"|" => (PIPE(yypos, yypos));

"!" => (NOT(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"hd" => (HEAD(yypos, yypos));
"tl" => (TAIL(yypos, yypos));
"ise" => (ISE(yypos, yypos));
"print" => (PRINT(yypos, yypos));

"&&" => (AND(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"*" => (MUL(yypos, yypos));
"/" => (DIV(yypos, yypos));
"!=" => (NEQ(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LEQ(yypos, yypos));
"::" => (CAT(yypos, yypos));

"var" => (VAR(yypos, yypos));
"fun" => (FUN(yypos, yypos));
"rec" => (REC(yypos, yypos));
"fn" => (ANON(yypos, yypos));
"end" => (END(yypos, yypos));

{name} => (NAME(yytext, yypos, yypos));
