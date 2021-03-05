(* Plc Parser Aux *)

(* Create the body of a function expression. *)
fun makeFunAux (n: int, xs: (plcType * string) list, e: expr): expr = (*if n < length xs
    then Let(#1(hd(xs)), Item(n, #2(hd(xs))), makeFunAux(n + 1, tl xs, e))
    else *)e;

(* Create the list of arguments of a function. *)
fun makeType (args: (plcType * string) list): plcType = BoolT
    (*List[map(fn (argType, argName) => argType) args];*)

(* Create a function expression. *)
fun makeFun (f: string, xs: (plcType * string) list, rt: plcType, e1: expr, e2: expr): expr =
  case xs of
      [] => Letrec(f, ListT [], "()", rt, e1, e2)
    | (t,x)::[] => Letrec(f, t, x, rt, e1, e2)
    | _ =>
      let
        val t = makeType xs
        val e1' = makeFunAux (1, xs, e1)
      in
        Letrec(f, t, "$list", rt, e1', e2)
      end;

(* Create a Anonymous function expression. *)
fun makeAnon (xs:(plcType * string) list, e:expr):expr =
  case xs of
      [] => Anon(ListT [], "()", e)
    | (t,x)::[] => Anon(t,x,e)
    | _ =>
      let
        val t = makeType xs
      in
        let
          val e' = makeFunAux (1, xs, e)
        in
          Anon(t,"$list",e')
        end
      end;
