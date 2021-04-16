(* Plc interpreter main file *)

use "PlcInterp.sml";
use "Parse.sml";

exception TypeError

val en: plcVal env = [];

fun run(e: expr) =
    (teval(e, [])
        handle EmptySeq => (printEmptySeq("Type error\n"); raise TypeError )
        | UnknownType => (printUnknownType("Type error\n"); raise TypeError )
        | NotEqTypes => (printNotEqTypes("Type error\n"); raise TypeError )
        | WrongRetType => (printWrongRetType("Type error\n"); raise TypeError )
        | DiffBrTypes => (printDiffBrTypes("Type error\n"); raise TypeError )
        | IfCondNotBool => (printIfCondNotBool("Type error\n"); raise TypeError )
        | NoMatchResults => (printNoMatchResults("Type error\n"); raise TypeError )
        | MatchResTypeDiff => (printMatchResTypeDiff("Type error\n"); raise TypeError )
        | MatchCondTypesDiff => (printMatchCondTypesDiff("Type error\n"); raise TypeError )
        | CallTypeMisM => (printCallTypeMisM("Type error\n"); raise TypeError )
        | NotFunc => (printNotFunc("Type error\n"); raise TypeError )
        | ListOutOfRange => (printListOutOfRange("Type error\n"); raise TypeError )
        | OpNonList => (printOpNonList("Type error\n"); raise TypeError );
    let
        val evalResult = val2string (eval (e, en))
            handle Impossible => (printImpossible("Runtime error"); "Error evaluating expression\n")
            | ValueNotFoundInMatch => (printValueNotFoundInMatch("Runtime error"); "Error evaluating expression\n")
            | HDEmptySeq => (printHDEmptySeq("Runtime error"); "Error evaluating expression\n")
            | TLEmptySeq => (printTLEmptySeq("Runtime error"); "Error evaluating expression\n")
            | NotAFunc => (printNotAFunc("Runtime error"); "Error evaluating expression\n")
            | SymbolNotFound => (printSymbolNotFound("Runtime error"); "Error evaluating expression\n")
    in
        (print ("> " ^ evalResult ^ "\n");
        evalResult)
    end) handle TypeError => "Type error"

fun runFile(file: string) = run(PlcFrontEnd.fromFile(file))

fun runCli() = startInterp();