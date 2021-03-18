(* Plc interpreter main file *)

use "PlcInterp.sml";

val en: 'a env = [];

fun run(e: expr) = print ("> " ^ val2string (eval (e, en)) ^ "\n");