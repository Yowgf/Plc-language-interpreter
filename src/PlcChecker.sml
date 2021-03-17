(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes (* *)
exception IfCondNotBool (* "If" condition received non-boolean *)
exception NoMatchResults (* *)
exception MatchResTypeDiff (* Match result type different *)
exception MatchCondTypesDiff (* Match condition type different *)
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList (* Operator non list *)
