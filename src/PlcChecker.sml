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

fun checkIntT(t: plcType): bool =
    case t of
      IntT => true
    | _ => false

fun checkBoolT(t: plcType): bool =
    case t of
      BoolT => true
    | _ => false

fun checkIntOrBoolT(t: plcType): bool =
    case t of
      IntT => true
    | BoolT => true
    | _ => false

fun checkListT(t: plcType): bool =
    case t of
      ListT x => true
    | _ => raise OpNonList

fun listType(ListT(t)): plcType list = t

fun nthType(ListT(t), pos): plcType = if List.length(t) >= pos then List.nth(t, pos - 1) else raise ListOutOfRange

fun listHeadType(ListT(t)): plcType = if List.length(t) > 0 then hd t else raise EmptySeq

fun listTailType(ListT(t)): plcType = if List.length(t) > 0 then ListT(tl t) else raise EmptySeq

fun checkTypesMatch(t1: plcType, t2: plcType): plcType = if t1 = t2 then t1 else raise NotEqTypes

fun checkMatchTypes(t: plcType, [], typeFn, en): plcType = raise NoMatchResults
  | checkMatchTypes(t: plcType, (NONE, e)::[], typeFn, en): plcType = typeFn (e, en)
  | checkMatchTypes(t: plcType, (SOME(opt), e)::l, typeFn, en): plcType = 
    let 
        val optT = typeFn (opt, en) (* IntT *)
        val retT = typeFn (e, en) (* BoolT *)
    in (
        if checkTypesMatch(t, optT) = t
            then if List.length(l) = 0 orelse retT = checkMatchTypes(t, l, typeFn, en)
                then retT
                else raise MatchResTypeDiff
            else raise MatchCondTypesDiff
    ) end

fun typesFromList([], typeFn, en): plcType list = []
  | typesFromList((e::l): expr list, typeFn, en): plcType list = typeFn(e, en)::typesFromList(l, typeFn, en)

fun checkPrim1(opName: string, t: plcType): plcType =
    case opName of
      "!" => if checkBoolT(t) then BoolT else raise CallTypeMisM
    | "-" => if checkIntT(t) then IntT else raise CallTypeMisM
    | "hd" => if checkListT(t) then listHeadType t else raise OpNonList
    | "tl" => if checkListT(t) then listTailType t else raise OpNonList
    | "ise" => if checkListT(t) then BoolT else raise OpNonList

fun checkPrim2(opName: string, t1: plcType, t2: plcType): plcType =
    case opName of
      "&&" => if checkBoolT(t1) andalso checkBoolT(t2) then BoolT else raise CallTypeMisM
    | "+" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "-" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "*" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "/" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "=" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | "!=" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | "<" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | "<=" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | "::" => if checkIntOrBoolT(t1) andalso checkListT(t2) then ListT(t1::listType(t2)) else raise CallTypeMisM

fun checkType(e: expr, en): plcType =
    case e of
      ConI x => IntT
    | ConB x => BoolT
    | ESeq t => t
    | Var(name) => lookup en name
    | Let(name, e1, e2) => checkType(e2, (name, checkType(e1, en))::en)
    (* | Letrec(name, argTypes, argIndicator, retType, fBody, e1) =>  *)
    | Prim1(opName, e1) => checkPrim1 (opName, checkType (e1, en))
    | Prim2(opName, e1, e2) => checkPrim2(opName, checkType (e1, en), checkType (e2, en))
    | If(cond, e1, e2) => if checkBoolT(checkType (cond, en)) then checkTypesMatch(checkType (e1, en), checkType (e2, en)) else raise IfCondNotBool
    | Match(cond, alts) => checkMatchTypes(checkType(cond, en), alts, checkType, en)
    | List(l) => ListT(typesFromList(l, checkType, en))
    | Item(pos, l) => nthType(checkType(l, en), pos)
    | Anon(argTypes, argIndicator, fBody) => checkType(fBody, en)