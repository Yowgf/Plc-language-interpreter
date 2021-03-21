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
use "ErrorMessages.sml";

use "PlcChecker.sml";

exception Impossible (* Nothing worked! *)
exception HDEmptySeq (* Head empty sequence *)
exception TLEmptySeq (* Tail empty sequence *)
exception ValueNotFoundInMatch
exception NotAFunc
exception QuitInterp
              
fun evalInt (IntV x) = x

fun evalBool (BoolV x) = x

fun listComponents (ListV x) = x

fun evalList ([], evalFn, en) = []
  | evalList ((h::t), evalFn, en) = (evalFn (h, en))::evalList(t, evalFn, en)

fun evalPrim1 (opName, x) =
    case opName of
        "!" => BoolV(not (evalBool x))
      | "-" => IntV(~ (evalInt x))
      | "hd" => (case x of
                    ListV([]) => raise HDEmptySeq
                  | ListV(l) => hd l
                  | _ => raise Impossible
                )
      | "tl" => (case x of
                    ListV([]) => raise TLEmptySeq
                  | ListV(l) => ListV(tl l)
                  | _ => raise Impossible
                )
      | "ise" => (case x of
                    ListV([]) => BoolV(true)
                  | ListV(l) => BoolV(false)
                  | _ => raise Impossible
                 )
      | "print" => (print (val2string x); ListV([]))

fun evalPrim2 (opName, x, y) =
    case opName of
        "&&" => BoolV(evalBool x andalso evalBool y)
      | "+" => IntV(evalInt x + evalInt y)
      | "-" => IntV(evalInt x - evalInt y)
      | "*" => IntV(evalInt x * evalInt y)
      | "/" => IntV(evalInt x div evalInt y)
      | "=" => BoolV(evalInt x = evalInt y)
      | "!=" => BoolV(evalInt x <> evalInt y)
      | "<" => BoolV(evalInt x < evalInt y)
      | "<=" => BoolV(evalInt x <= evalInt y)
      | "::" => ListV(x :: listComponents (y))

fun matchResult (v, [], evalFn, en): expr = raise ValueNotFoundInMatch
  | matchResult (v, (NONE, e)::[], evalFn, en): expr = e
  | matchResult (v, (SOME(e1), e2)::l, evalFn, en): expr =
    case v of
        IntV(x) => if x = evalInt (evalFn (e1, en)) then e2 else matchResult(v, l, evalFn, en)
      | BoolV(x) => if x = evalBool (evalFn (e1, en)) then e2 else matchResult(v, l, evalFn, en)

(*
Precisamos do corpo da funcao ser avaliado antes de adiciona-lo ao
ambiente. Ao mesmo tempo, precisamos te-lo no ambiente antes de no
ambiente antes de avalia-lo.
*)
fun evalCall (Clos(fName, argIndicator, fBody, fSt), fArgs, evalFun, en) =
    (* evalFun(fBody, (fName, Clos("", argIndicator, fBody, en))::(argIndicator, fArgs)::en) *)
    evalFun(fBody, (argIndicator, fArgs)::en)
  | evalCall _ = raise NotAFunc

fun eval (e, en) =(
    case e of
        ConI(x) => IntV(x)
      | ConB(x) => BoolV(x)
      | ESeq(_) => SeqV([])
      | Var(name) => lookup en name
      | Let(name, e1, e2) => eval(e2, (name, eval(e1, en))::en)
      | Letrec(fName, argTypes, argIndicator, retType, e1, e2) =>
        eval(e2, (fName, Clos(fName, argIndicator, e1, en))::en)
      | Prim1(opName, e1) => evalPrim1(opName, eval (e1, en))
      | Prim2(opName, e1, e2) =>
        (case opName of
             ";" => (eval(e1, en); eval(e2, en))
           | _ => evalPrim2(opName, eval (e1, en), eval (e2, en))
        )
      | If(cond, e1, e2) => if evalBool (eval (cond, en)) then eval (e1, en) else eval (e2, en)
      | Match(e1, alts) => eval (matchResult(eval (e1, en), alts, eval, en), en)
      | Call(f, fArgs) => evalCall(eval(f, en), eval(fArgs, en), eval, en)
      | List(l) => ListV(evalList(l, eval, en))
                        
      | Item(pos, l) =>
        (List.nth(listComponents(eval (l, en)), pos - 1)
         (* According to instructor's recomendations, raise Impossible *)
         handle Subscript => raise Impossible)
      (* Maybe will bug once type checking is needed *)
      | Anon(argTypes, argIndicator, fBody) => Clos("", argIndicator, fBody, en)
)

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
            ( 
              checkType(parserOutput, []);
              print ("> " ^ val2string (eval (parserOutput, en)) ^ "\n\n");
              interp (true, en)
            )
            handle QuitInterp => interp (false, en)
                 | ValueNotFoundInMatch => (printValueNotFoundInMatch(input); interp (true, en))
                 | HDEmptySeq => (printHDEmptySeq(input); interp (true, en))
                 | TLEmptySeq => (printTLEmptySeq(input); interp (true, en))
                 | NotAFunc => (printNotAFunc(input); interp (true, en))
                 | SymbolNotFound => (printSymbolNotFound(input); interp (true, en))
                 | EmptySeq => (printEmptySeq(input); interp (true, en))
                 | NotEqTypes => (printNotEqTypes(input); interp (true, en))
                 | IfCondNotBool => (printIfCondNotBool(input); interp (true, en))
                 | NoMatchResults => (printNoMatchResults(input); interp (true, en))
                 | MatchResTypeDiff => (printMatchResTypeDiff(input); interp (true, en))
                 | MatchCondTypesDiff => (printMatchCondTypesDiff(input); interp (true, en))
                 | CallTypeMisM => (printCallTypeMisM(input); interp (true, en))
                 | ListOutOfRange => (printListOutOfRange(input); interp (true, en))
                 | OpNonList => (printOpNonList(input); interp (true, en))

        end
    else 0;

fun startInterp () = interp(true, []);
