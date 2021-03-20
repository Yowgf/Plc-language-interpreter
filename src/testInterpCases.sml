(* Just a list of test cases and expected values for the interpreter
 *)

(* Return of 0 indicates failure *)
fun test [] = 1
  | test ((code: string, exptRes: string)::casesTail) =
    if (run (parseInput code)) = exptRes
       handle _ => (print "Uncaught exception while testing!\n";
                    false)
    then
        test casesTail
    else
        (printTestError code; 0) (* printTestError comes from parser
                                    testing framework *)

(* If this function returns false, then the test failed (e.g.
   running the code didn't raise the expected exception)
   This function is different from "test", in that it has to handle
   the exceptions thrown. It was decided to keep these two kinds of
   tests separate.
*)
fun failWith (code, except: string) =
    (run (parseInput code); true)
    handle
         (* Interpreter exceptions *)
           ValueNotFoundInMatch => except = "ValueNotFoundInMatch"
                                
         (* Type exceptions *)
         | EmptySeq => except = "EmptySeq"
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

(* Return of 0 indicates failure *)
fun testExceptions [] = 1
  | testExceptions ((code, exptExn)::casesTail) =
    (failWith(code, exptExn); testExceptions casesTail)
    handle _ => (printTestError code; 0)
            
val interpCases =
    let val s = "0"
        val e = "0"
    in
        (s, e)
    end ::
    let val s = "true"
        val e = "true"
    in
        (s, e)
    end ::
    (* Don't move this last case *)
    let val s = "0"
        val e = "0"
    in
        (s, e)
    end :: []

val exceptionCases =
    (* Fail with exception tests *)
    (* Don't move this last case *)
    let val s = "match 0 with | 1 -> 1 end"
        val e = "ValueNotFoundInMatch"
    in
        (s, e)
    end :: []
