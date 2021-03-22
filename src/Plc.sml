(* Plc interpreter main file *)

use "PlcInterp.sml";

val en: plcVal env = [];

fun run(e: expr) =
    (teval(e, []);
    let
        val evalResult = val2string (eval (e, en))
    in
        (print ("> " ^ evalResult ^ "\n");
         evalResult)
    end)
