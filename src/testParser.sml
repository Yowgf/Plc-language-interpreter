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

(* Const *)
fromString "true";
fromString "false";
fromString "500";
fromString "()";
fromString "(1, 2, 3)";

(* Types *)
fromString "(Int [])";
fromString "(Bool [])";
fromString "(Nil [])";
fromString "(Int -> Bool -> Nil [])";
fromString "((Int -> Int) -> Bool [])";

(* Unary operators *)
fromString "!42";
fromString "-true";
fromString "hd ()";
fromString "tl 900";
fromString "ise ()";
fromString "print 10001";

(* Anonymous function *)
fromString "fn (Int a) => a end";
fromString "fn (Int a, Nil b) => a end";
fromString "fn (Int x) => x end";
fromString "fn () => () end";

(* Declarations *)
(* Function declaration *)
fromString "fun myFunc(Int a, Bool b) = 10; myFunc";
fromString "fun myFunc2(Int a, Nil b) = a; myFunc2";
fromString "fun myFunc3() = d; myFunc3";
fromString "fun myFunc4() = d; fun myFunc5() = e; f";

(* Variable declaration *)
fromString "var x = 10; x";
fromString "var a = 50; var b = 70; c";
fromString "var b = !false; p";

(* Conditionals *)
fromString "if myVar then a else b";
fromString ("if myVar then if true then () else false else" ^
            " fn (Int i) => i end");

(* Special nested stuff *)
fromString "fun invert(Int a) = { var x = -a; x }; 0";
    
(*
fromString "15";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])";
fromString "print x; true";
fromString "3::7::t";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 3";
fromString "fun f(Int x) = x; f(1)";
fromString "match x with | 0 -> 1| _ -> -1 end";
fromFile ("example.plc");

use "testParserCases.sml"
*)

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)
