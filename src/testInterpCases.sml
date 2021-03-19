(* Just a list of test cases and expected values for the interpreter
 *)

(*

How to test exception throwing?

*)

(* If this function returns false, then the test failed (e.g.
   running the code didn't raise the expected exception) *)
fun failsWith (code, except: string) =
    (run code; true)
    handle
         (* Type exceptions *)
           EmptySeq => except = "EmptySeq"
         | UnknownType => except = "UnknownType"
         | NotEqTypes => except = "NotEqTypes"
         | WrongRetType => except = "WrongRetType"
         | DiffBrTypes => except = "DiffBrTypes"
         | IfCondNotBool => except = "IfCondNotBool"
         | NoMatchResults => except = "NoMatchResults"
         | MatchResTypeDiff => except = "MatchResTypeDiff"
         | MatchCondTypesDiff => except = "MatchCondTypesDiff"
         | CallTypeMisM => except = "CallTypeMis"
         | NotFunc => except = "NotFunc"
         | ListOutOfRange => except = "ListOutOfRange"
         | OpNonList => except = "OpNonList"

fun printTestFailure (testExpr) =
    (TextIO.output(TextIO.stdOut, "Error in some" ^
                                  " test in case...\n\n");
                   testExpr)

fun test [] = ConI 0
  | test ((inExpr: expr, exptRes: string)::casesTail) =
    if (run inExpr) = exptRes
    then
        test casesTail
    else
        printTestFailure inExpr
        handle _ => (print ("Uncaught exception while testing" ^
                            " interpreter!\n");
                     printTestFailure inExpr)

val interpCases =
    let val s = ConI 0
        val e = "0"
    in
        (s, e)
    end ::
    let val s = ConB true
        val e = "true"
    in
        (s, e)
    end ::
    (* Don't move this last case *)
    let val s = ConI 0
        val e = "0"
    in
        (s, e)
    end :: []
