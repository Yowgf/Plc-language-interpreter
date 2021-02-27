(* Plc Lexer *)

(* User declarations *)

datatype lexresult= NAME of string |  NUM of int

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

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

%%

%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));

progr       = {exp} | ({decl} ";" {progr});

decl        = (var {name} = {exp})
            | (fun {name} {args} = {exp})
            | (fun rec {name} {args}: {type} = {exp});

exp         = {atomic_exp}
            | {app_exp}
            | (if {exp} then {exp} else {exp})
            | (match {exp} with {match_exp})
            | (! {exp})
            | (- {exp})
            | (hd {exp})
            | (tl {exp})
            | (ise {exp})
            | (print {exp})
            | ({exp} && {exp})
            | ({exp} "+" {exp})
            | ({exp} - {exp})
            | ({exp} "*" {exp})
            | ({exp} "/" {exp})
            | ({exp} "=" {exp})
            | ({exp} "!=" {exp})
            | ({exp} "<" {exp})
            | ({exp} "<=" {exp})
            | ({exp} :: {exp})
            | ({exp} ";" {exp})
            | ({exp} "["{nat}"]");

atomic_exp  = {const}
            | {name}
            | ("{" {progr} "}")
            | ("(" {exp} ")")
            | ("(" {comps} ")")
            | (fn {args} "=>" {exp} end);

app_exp     = ({atomic_exp} {atomic_exp})
            | ({app_exp} {atomic_exp});

const       = "true" | "false" | {nat} | "( )" | ("("{type} "[])");

comps       = ({exp} , {exp})
            | ({exp} , {comps});

match_exp   = "end"
            | ("|" {cond_exp} "->" {exp} {match_exp});

cond_exp    = {exp}
            | _;

args        = "( )"
            | ("(" {params} ")");

params      = {typed_var}
            | ({typed_var}, {params});

typed_var   = {type} {name};

type        = {atomic_type}
            | ("(" {types} ")")
            | ("[" {type} "]")
            | ({type} "->" {type});

atomic_type = "Nil"
            | "Bool"
            | "Int"
            | ("(" {type} ")")

types       = ({type}, {type})
            | ({type}, {types});

name        = [a-zA-Z_][a-zA-Z_0-9]*;

nat         = [0-9]+;

white_space = [\ \t];

%%

\n => (inc lineNumber; lex());
{white_space} => (lex());
{nat} => (NUM (revfold (fn(a,r)=>ord(a)-ord("0")+10*r) (explode yytext) 0))
