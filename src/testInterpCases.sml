(* Just a list of test cases and expected values for the interpreter
 *)

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
