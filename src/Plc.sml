(* Plc interpreter main file *)

use "PlcInterp.sml";

val en: 'a env = [];

fun run(e: expr) =
    let
        val evalResult = val2string (eval (e, en))
    in
        (print ("> " ^ evalResult ^ "\n");
         evalResult)
    end
