(* Test the parser. If there is an error it will propagate, stoping
   the script.
 *)
use "testParser.sml";

(* Import the Plc interpreter routines *)
use "Plc.sml";

use "testInterpCases.sml";
testInterpFramework.testAll()
