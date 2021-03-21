fun printInvalidInput input =
    TextIO.output(TextIO.stdOut, "\nInvalid syntax: \n***\n" ^
                                 input ^ "***\n\n")

fun printValueNotFoundInMatch input =
    TextIO.output(TextIO.stdOut, "\nValue not found in match: \n***\n" ^
                                 input ^ "***\n\n")

fun printHDEmptySeq input =
    TextIO.output(TextIO.stdOut, "\nEmpty sequence provided to hd operator: \n***\n" ^
                                 input ^ "***\n\n")

fun printTLEmptySeq input =
    TextIO.output(TextIO.stdOut, "\nEmpty sequence provided to tl operator: \n***\n" ^
                                 input ^ "***\n\n")

fun printNotAFunc input =
    TextIO.output(TextIO.stdOut, "\nCall to non-functional entity:\n***\n" ^
                                 input ^ "***\n\n")

fun printSymbolNotFound input =
    TextIO.output(TextIO.stdOut, "\nSymbol not found:\n***\n" ^
                                 input ^ "***\n\n")

fun printCallTypeMisM input =
    TextIO.output(TextIO.stdOut, "\nCall types mismatch:\n***\n" ^
                                 input ^ "***\n\n")

fun printIfCondNotBool input =
    TextIO.output(TextIO.stdOut, "\nIf condition is not bool:\n***\n" ^
                                 input ^ "***\n\n")

fun printOpNonList input =
    TextIO.output(TextIO.stdOut, "\nUsing operator in non-list entity:\n***\n" ^
                                 input ^ "***\n\n")
