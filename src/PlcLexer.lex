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

nat = [0-9]+;
ws  = [\ \t]+;
name = [a-zA-Z_][a-zA-Z0-9_]*;

%%

{nat} => (integer(yytext, yypos));
{ws} => (lex());
"true" => (TRUE(true, yypos, yypos));
"false" => (FALSE(false, yypos, yypos));
"(" => (LPAREN(yypos, yypos));
")" => (RPAREN(yypos, yypos));
"[" => (LBRAC(yypos, yypos));
"]" => (RBRAC(yypos, yypos));
"=" => (EQ(yypos, yypos));
"Int" => (INTT(IntT, yypos, yypos));
"Bool" => (BOOLT(BoolT, yypos, yypos));
"Nil" => (NILT(ListT([]), yypos, yypos));
"var" => (LET(yypos, yypos));
{name} => (NAME(yytext, yypos, yypos));
