(* Just a list of test cases and expected values for the interpreter
 *)

(* Return of 0 indicates failure *)
fun test [] = 1
  | test ((code: string, exptRes: string)::casesTail) =
    if (run (parseInput code)) = exptRes
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
    (* Constants *)
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
    let val s = "([Bool] [])"
        val e = "[]"
    in
        (s, e)
    end ::
    (* Prim1 operators *)
    let val s = "!false"
        val e = "true"
    in
        (s, e)
    end ::
    let val s = "-1"
        val e = "~1"
    in
        (s, e)
    end ::
    (* Prim2 operators *)
    let val s = "true && false"
        val e = "false"
    in
        (s, e)
    end ::
    let val s = "true && true"
        val e = "true"
    in
        (s, e)
    end ::
    let val s = "5 + 6"
        val e = "11"
    in
        (s, e)
    end ::
    let val s = "10 - 5"
        val e = "5"
    in
        (s, e)
    end ::
    let val s = "42 * 24"
        val e = "1008"
    in
        (s, e)
    end ::
    let val s = "42 / 24"
        val e = "1"
    in
        (s, e)
    end ::
    let val s = "0 = 1"
        val e = "false"
    in
        (s, e)
    end ::
    let val s = "0 != 1"
        val e = "true"
    in
        (s, e)
    end ::
    let val s = "400 < 400"
        val e = "false"
    in
        (s, e)
    end ::
    let val s = "400 <= 400"
        val e = "true"
    in
        (s, e)
    end ::
    (* Let, var and functions *)
    let val s = "var x = 5; x; x"
        val e = "5"
    in
        (s, e)
    end ::
    let val s = "fun mul(Int a, Int b) = a * b; mul(mul(5, 5), 5)"
        val e = "125"
    in
        (s, e)
    end ::
    let val s = "(fn () => () end)()"
        val e = "()"
    in
        (s, e)
    end ::
    let val s = "fun rec sumTo(Int a): Int = if a != 0 then a + sumTo(a - 1) else 0; sumTo(5)"
        val e = "15"
    in
        (s, e)
    end ::
    let val s = "0"
        val e = "0"
    in
        (s, e)
    end :: []

val exceptionCases =
    (* Fail with exception tests *)
    let val s = "match 0 with | 1 -> 1 end"
        val e = "ValueNotFoundInMatch"
    in
        (s, e)
    end :: []
