Control.Print.printDepth := 1;
Control.Print.out := {
    say=fn _=>(),
    flush=fn()=>()
};

val _ = CM.make("$/basis.cm");
val _ = CM.make("$/ml-yacc-lib.cm");

val _ = use "Plc.sml";
val _ = use "PlcInterp.sml";
val _ = use "Parse.sml";

val args = CommandLine.arguments()

fun start() = if (length args) > 0 
    then (runFile(List.nth(args, 0)); 0)
    else runCli();

start();
val _ = OS.Process.exit(OS.Process.success)