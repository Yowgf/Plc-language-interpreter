(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

(* Add functions that might help for testing in Parse.sml *)
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

(* Remember to test:
   - names that end with digits
   - cases where the program should throw exception (put these in
     testParserCases.sml)
*)

exception ParserTestFailure

(* Raises test failure *)
fun fail() = raise ParserTestFailure;

(* Fails is file parse succeeds *)
fun expectFileToFail(file: string) = let val e = (fromFile (file) handle  ParseError => ConI 0) in
   if e = ConI 0 then e else fail()
end;

(* Fails is string parse succeeds *)
fun expectStringToFail(code: string) = let val e = (fromString (code) handle  ParseError => ConI 0) in
   if e = ConI 0 then e else fail()
end;

(* Const *)
fromString "true";
fromString "false";
fromString "15";
fromString "500";
fromString "()";
fromString "(1, 2, 3)";
fromString "(6,false)[1]";
fromString "()";

(* Types *)
fromString "(Int [])";
fromString "(Bool [])";
fromString "(Nil [])";
fromString "(Int -> Bool -> Nil [])";
fromString "((Int -> Int) -> Bool [])";
fromString "((Int, Bool) [])";
fromString "((Int, Int) -> Bool [])";
fromString ("((Bool -> Int, Nil -> Bool, Int -> Nil) -> Nil -> " ^
            "(Nil, Bool, Int) [])");
fromString "([Int] [])";
fromString "([(Bool, Int)] [])";
expectStringToFail "([(Bool,)] [])";

(* Unary operators *)
fromString "!42";
fromString "-true";
fromString "hd ()";
fromString "tl 900";
fromString "ise ()";
fromString "print 10001";
fromString "print x; true";

(* Binary operators *)
fromString "(true && false)";
fromString "(a + b)";
fromString "var x = x + y; x * y";
fromString ("fun sum(Int a, Int b) = { var s = a + b; s }; " ^
            "{ sum(1, 0) } && false");
fromString "(a - b)";
fromString "(a / b)";
fromString "fun double(Int a) = a * 2; { var a = 4; double(a) }";
fromString "(a = b)";
fromString "(a != b)";
fromString "(a < b)";
fromString "(a <= b)";
fromString "3::7::t";
fromString "(3::7::t)";
(* Test Precedence *)
fromString ("a; b; if a then if b then c else d else a && b && " ^
           "a = b = c != a != a :: b :: a + b + a * b * a / b / 1");

(* Anonymous function *)
fromString "fn (Int a) => a end";
fromString "fn (Int a, Nil b) => a end";
fromString "fn (Int x) => x end";
fromString "fn () => () end";
fromString "fn (Int x) => -x end";

(* Declarations *)
(* Function declaration *)
fromString "fun myFunc(Int a, Bool b) = 10; myFunc";
fromString "fun myFunc2(Int a, Nil b) = a; myFunc2";
fromString "fun myFunc3() = d; myFunc3";
fromString "fun myFunc4() = d; fun myFunc5() = e; f";
fromString "fun f(Int x) = x; f(1)";
(* Function application precedence *)
fromString "fun invert(Int a) = -a; print invert a";

(* Variable declaration *)
fromString "var x = 10; x";
fromString "var a = 50; var b = 70; c";
fromString "var b = !false; p";
fromString "var x = 9; x + 3";
expectStringToFail "var x = 10";
expectStringToFail "var x = 10;";

(* Conditionals *)
fromString "if myVar then a else b";
fromString ("if myVar then if true then () else false else" ^
            " fn (Int i) => i end");
fromString "match x with | 0 -> 1| _ -> -1 end";
fromFile ("../tests/matchExpr.plc");
expectFileToFail("../tests/incorrectMatch.plc");

(* Special nested stuff *)
fromString "fun invert(Int a) = { var x = -a; x }; Invert(50)";
fromString "fun minus(Int a, Int b) = a - b; minus(42, 21)";
fromString "(6, false)[1]";

(* Recursive funtions *)
fromFile ("../tests/example.plc");
fromFile ("../tests/fat.plc");
fromFile ("../tests/fibonacci.plc");

(* use "testParserCases.sml"; *)

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)
