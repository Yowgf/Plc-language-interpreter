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

fun checkStringT(t: plcType): bool =
    case t of
      StringT => true
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

fun checkFunT(t: plcType): bool =
    case t of
      FunT x => true
    | _ => false

fun checkComparable(t: plcType): bool =
    case t of
      FunT _ => false
    | _ => true

fun listType(ListT(t)): plcType list = t
  | listType _ = raise UnknownType

fun nthType(ListT(t), pos): plcType = if List.length(t) >= pos then List.nth(t, pos - 1) else raise ListOutOfRange
  | nthType _ = raise UnknownType

fun listHeadType(ListT(t)): plcType = if List.length(t) > 0 then hd t else raise EmptySeq
  | listHeadType _ = raise UnknownType

fun listTailType(ListT(t)): plcType = if List.length(t) > 0 then ListT(tl t) else raise EmptySeq
  | listTailType _ = raise UnknownType

fun checkTypesListMatch([], [], _) = true
  | checkTypesListMatch((t1::l1), (t2::l2), compareFn) = (compareFn(t1, t2); checkTypesListMatch(l1, l2, compareFn))
  | checkTypesListMatch _ = raise NotEqTypes

fun checkTypesMatch(IntT, IntT): plcType = IntT
  | checkTypesMatch(BoolT, BoolT): plcType = BoolT
  | checkTypesMatch(FunT(a1,r1), FunT(a2, r2)): plcType = (checkTypesMatch(a1, a2); checkTypesMatch(r1, r2); FunT(a1, r1))
  | checkTypesMatch(ListT(t1), ListT(t2)): plcType = if checkTypesListMatch(t1, t2, checkTypesMatch) then ListT(t1) else raise NotEqTypes
  | checkTypesMatch(SeqT(t1), SeqT(t2)): plcType = SeqT(checkTypesMatch(t1, t2))
  | checkTypesMatch _ = raise NotEqTypes

fun checkMatchTypes(t: plcType, [], teval, en): plcType = raise NoMatchResults
  | checkMatchTypes(t: plcType, (NONE, e)::[], teval, en): plcType = teval (e, en)
  | checkMatchTypes(t: plcType, (SOME(opt), e)::l, teval, en): plcType = 
    let 
        val optT = teval (opt, en) (* IntT *)
        val retT = teval (e, en) (* BoolT *)
    in (
            (
              checkTypesMatch(t, optT);
              if List.length(l) = 0 orelse retT = checkMatchTypes(t, l, teval, en)
                then retT
                else raise MatchResTypeDiff
            ) handle NotEqTypes => raise MatchCondTypesDiff
    ) end
  | checkMatchTypes _ = raise UnknownType

fun typesFromList([], teval, en): plcType list = []
  | typesFromList((e::l): expr list, teval, en): plcType list = teval(e, en)::typesFromList(l, teval, en)

fun checkPrim1(opName: string, t: plcType): plcType =
    case opName of
      "!" => if checkBoolT(t) then BoolT else raise CallTypeMisM
    | "-" => if checkIntT(t) then IntT else raise CallTypeMisM
    | "hd" => if checkListT(t) then listHeadType t else raise OpNonList
    | "tl" => if checkListT(t) then listTailType t else raise OpNonList
    | "ise" => if checkListT(t) then BoolT else raise OpNonList
    | "print" => ListT([])
    | _ => raise UnknownType

fun checkPrim2(opName: string, t1: plcType, t2: plcType): plcType =
    case opName of
      "&&" => if checkBoolT(t1) andalso checkBoolT(t2) then BoolT else raise CallTypeMisM
    | "||" => if checkBoolT(t1) andalso checkBoolT(t2) then BoolT else raise CallTypeMisM
    | "+" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "-" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "*" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "/" => if checkIntT(t1) andalso checkIntT(t2) then IntT else raise CallTypeMisM
    | "=" =>  if checkComparable(t1) andalso checkComparable(t2) 
                then (checkTypesMatch(t1, t2); BoolT) handle NotEqTypes => raise CallTypeMisM
                else raise CallTypeMisM
    | "!=" => if checkComparable(t1) andalso checkComparable(t2) 
                then (checkTypesMatch(t1, t2); BoolT) handle NotEqTypes => raise CallTypeMisM
                else raise CallTypeMisM
    | "<" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | ">" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | "<=" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | ">=" => if checkIntT(t1) andalso checkIntT(t2) then BoolT else raise CallTypeMisM
    | "::" => if checkIntOrBoolT(t1) andalso checkListT(t2) then ListT(t1::listType(t2)) else raise CallTypeMisM
    | ";" => t2
    | _ => raise UnknownType

fun checkCallType(FunT(argTypes, retType), fArgs) = (
      case argTypes of
        ListT(t) => (
          (checkTypesListMatch(t, listType fArgs, checkTypesMatch); retType) 
            handle NotEqTypes => raise CallTypeMisM
                | _ => raise UnknownType
        )
      | _ => (
        (checkTypesMatch(argTypes, fArgs); retType) 
          handle NotEqTypes => raise CallTypeMisM
        | _ => raise UnknownType
      )
    )
  | checkCallType _ = raise UnknownType

fun checkRecFun(name, argTypes, argIndicator, retType, fBody, teval, en) = 
  FunT(argTypes, checkTypesMatch(retType, teval(fBody, (name, FunT(argTypes, retType))::(argIndicator, argTypes)::en)) handle NotEqTypes => raise WrongRetType)

fun teval(e: expr, en): plcType =
    case e of
      ConI x => IntT
    | ConB x => BoolT
    | ConS x => StringT
    | ESeq t => t
    | Var(name) => lookup en name
    | Let(name, e1, e2) => teval(e2, (name, teval(e1, en))::en)
    | Letrec(name, argTypes, argIndicator, retType, fBody, e1) => teval(e1, (name, checkRecFun(name, argTypes, argIndicator, retType, fBody, teval, en))::en)
    | Prim1(opName, e1) => checkPrim1 (opName, teval (e1, en))
    | Prim2(opName, e1, e2) => checkPrim2(opName, teval (e1, en), teval (e2, en))
    | If(cond, e1, e2) => 
        if checkBoolT(teval (cond, en)) 
          then (checkTypesMatch(teval (e1, en), teval (e2, en))) handle NotEqTypes => raise DiffBrTypes
          else raise IfCondNotBool
    | Match(cond, alts) => checkMatchTypes(teval(cond, en), alts, teval, en)
    | Call(f, fArgs) => let
        val fnType = teval(f, en) 
      in 
        if checkFunT fnType 
          then checkCallType(fnType, teval(fArgs, en))
          else raise NotFunc
      end
    | List(l) => ListT(typesFromList(l, teval, en))
    | Item(pos, l) => nthType(teval(l, en), pos)
    | Anon(argTypes, argIndicator, fBody) => FunT(argTypes, teval(fBody, (argIndicator, argTypes)::en))
