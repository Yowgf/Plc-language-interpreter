(* PlcInterp *)
CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

exception Impossible (* Nothing worked! *)
exception HDEmptySeq (* Head empty sequence *)
exception TLEmptySeq (* Tail empty sequence *)
exception ValueNotFoundInMatch
exception NotAFunc
exception QuitInterp

(*

@TODO expression "4 4 4" is invalid syntax but does not yield
      error.

@TODO create interpreting function "run", according to
      specification.

*)
              
fun evalInt (IntV x) = x

fun evalBool (BoolV x) = x
                           
fun evalPrim2 (opName, x, y) =
    case opName of
        "&&" => BoolV(evalBool x andalso evalBool y)
      | "+" => IntV(evalInt x + evalInt y)
      | "-" => IntV(evalInt x - evalInt y)
      | "*" => IntV(evalInt x * evalInt y)
      | "/" => IntV(evalInt x div evalInt y)
      | "!=" => BoolV(evalInt x <> evalInt y)
      | "<" => BoolV(evalInt x < evalInt y)
      | "<=" => BoolV(evalInt x <= evalInt y)
                           
fun evalPrim1 (opName, x) =
    case opName of
        "!" => BoolV(not (evalBool x))
      | "-" => IntV(~ (evalInt x))
                           
fun eval (e, en) =
    case e of
        ConI(x) => IntV(x)
      | ConB(x) => BoolV(x)
      | Var(name) => lookup en name
      | Let(name, e1, e2) => (en = (name, eval (e1, en))::en; eval (e2, en))
      | Prim2(opName, e1, e2) => evalPrim2(opName, eval (e1, en), eval (e2, en))
      | Prim1(opName, e1) => evalPrim1(opName, eval (e1, en))
      | If(cond, e1, e2) => if evalBool (eval (cond, en)) then eval (e1, en) else eval (e2, en)

fun printInvalidInput input =
    TextIO.output(TextIO.stdOut, "Invalid syntax: \n\n***\n" ^
                                 input ^ "***\n\n")

fun parseInput input =
    if input = ":quit\n" then raise QuitInterp
    else PlcFrontEnd.fromString input
    handle ParseError => (printInvalidInput input; ConI 1)
                                          
fun interp (isInterpreting, en)  =
    if isInterpreting then
        let
            val input = (print "< "; TextIO.input TextIO.stdIn)
            val parserOutput = parseInput input
        in
            (print ("> " ^ val2string (eval (parserOutput, en)) ^ "\n");
             interp (true, en))
            handle QuitInterp => interp (false, en)
                 | _ => interp (true, en)
        end
    else 0;

fun startInterp () = interp(true, []);
