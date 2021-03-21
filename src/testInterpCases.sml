(* Just a list of test cases and expected values for the interpreter
 *)
structure testInterpFramework =
struct
fun printTestFailure (code: string, expected: string, result: string) =
    TextIO.output(TextIO.stdOut, "\nFailure of test:\n***\n" ^ code ^
                                 "\n***\nExpected:\n***\n" ^ expected ^
                                 "\n***\nGot:\n***\n" ^ result ^
                                 "\n***\n\n")

(* Return of 0 indicates failure *)
fun test [] = 1
  | test ((code: string, exptRes: string)::casesTail) =
    let
        val runRes = run (parseInput code)
    in
        if (run (parseInput code)) = exptRes
        then
            test casesTail
        else
            (printTestFailure(code, exptRes, runRes); 0)
    end

(* Expect code to fail. Just converts the code into its result. If
an exception is thrown, convert the exception into its string
representation to be able to check if it matches the expected
exception.
 *)
fun exptFail code =
    run (parseInput code)
    handle
    (* Interpreter exceptions *)
    Impossible => "Impossible"
    | HDEmptySeq => "HDEmptySeq"
    | TLEmptySeq => "TLEmptySeq"
    | ValueNotFoundInMatch => "ValueNotFoundInMatch"
    | NotAFunc => "NotAFunc"
    (* Type exceptions *)
    | EmptySeq => "EmptySeq"
    | UnknownType => "UnknownType"
    | NotEqTypes => "NotEqTypes"
    | WrongRetType => "WrongRetType"
    | DiffBrTypes => "DiffBrTypes"
    | IfCondNotBool => "IfCondNotBool"
    | NoMatchResults => "NoMatchResults"
    | MatchResTypeDiff => "MatchResTypeDiff"
    | MatchCondTypesDiff => "MatchCondTypesDiff"
    | CallTypeMisM => "CallTypeMis"
    | NotFunc => "NotFunc"
    | ListOutOfRange => "ListOutOfRange"
    | OpNonList => "OpNonList"
    | _ => "Test threw unknown exception!"

(* Return of 0 indicates failure *)
fun testExceptions [] = 1
  | testExceptions ((code, exptExn)::casesTail) =
    let
        val runRes = exptFail code
    in
        if runRes = exptExn
        then testExceptions casesTail
        else
            (printTestFailure(code, exptExn, runRes); 0)
    end
        
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
    let val s = "hd (1, 2)"
        val e = "1"
    in
        (s, e)
    end ::
    let val s = "tl (1, 2)"
        val e = "(2, )"
    in
        (s, e)
    end ::
    let val s = "ise ()"
        val e = "true"
    in
        (s, e)
    end ::
    let val s = "ise (3, 4, 5, 6)"
        val e = "false"
    in
        (s, e)
    end ::
    let val s = "print 101"
        val e = "()"
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
    let val s = "1 :: (2, 3)"
        val e = "(1, 2, 3, )"
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
    let val s = "(10, 12, 13)[4]"
        val e = "Impossible"
    in
        (s, e)
    end ::
    let val s = "hd ()"
        val e = "HDEmptySeq"
    in
        (s, e)
    end ::
    let val s = "tl ()"
        val e = "TLEmptySeq"
    in
        (s, e)
    end ::
    let val s = "match 0 with | 1 -> 1 end"
        val e = "ValueNotFoundInMatch"
    in
        (s, e)
    end ::
    let val s = "7()"
        val e = "NotAFunc"
    in
        (s, e)
    end :: []

(* Helps printTestingResults count number of succ/fails *)
fun printTestingResultsAux (h::t, acc) =
    printTestingResultsAux (t, acc + h)
  | printTestingResultsAux ([], acc) = acc

(* Prints number of successes and failures *)
fun printTestingResults (testResList) =
    let
        val numSuccess = printTestingResultsAux (testResList, 0)
        val numFailures = (List.length testResList) - numSuccess
    in
        (print "\n\nTest results:\n";
         print ((Int.toString numSuccess) ^ " tests succeded\n");
         print ((Int.toString numFailures) ^ " tests failed\n\n"))
    end

(* Process the test while printing useful information *)
fun processSuccessFailure (testName, testFun, testCases) =
    let
        val testRes = (print ("Testing " ^ testName ^ "...\n");
                       testFun testCases)
    in
        (if testRes = 1 then
             print ("Test " ^ testName ^ " SUCCESS.\n")
         else
             print ("Test " ^ testName ^ " FAILURE.\n")
        ; testRes)
    end
        
(* Tests everything and prints results *)
fun testAll () =
    let
        val testRes = processSuccessFailure ("normal", test, interpCases)
        val testExnRes = processSuccessFailure ("exceptions", testExceptions, exceptionCases)
    in
        printTestingResults([testRes, testExnRes])
    end

end (* End testInterpFramework structure *)
        
