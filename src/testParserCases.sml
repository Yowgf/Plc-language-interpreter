fun equivalentType (IntT, IntT) = true
  | equivalentType (BoolT, BoolT) = true
  | equivalentType (FunT (t11, t12), FunT (t21, t22)) =
    equivalentType (t11, t21) andalso equivalentType (t12, t22)
  | equivalentType (ListT (h1::t1), ListT(h2::t2)) =
    equivalentType(h1, h2) andalso equivalentType(ListT t1, ListT t2)
  | equivalentType (ListT [], ListT (h::t)) = false
  | equivalentType (ListT (h::t), ListT []) = false
  | equivalentType (ListT [], ListT []) = true
  | equivalentType (SeqT t1, SeqT t2) = equivalentType(t1, t2)
  | equivalentType (_, _) = false

fun equivalent (inExpr: expr, expectedExpr: expr) =
    case (inExpr, expectedExpr) of
        (ConI a, ConI b) => a = b
      | (ConB a, ConB b) => a = b
      | (ESeq a, ESeq b) => equivalentType(a, b)
      | (Var str1, Var str2) => str1 = str2
      | (Let (str1, e11, e12), Let (str2, e21, e22)) =>
        str1 = str2 andalso
        equivalent(e11, e21) andalso equivalent(e12, e22)
      | (Letrec (str11, t11, str12, t12, e11, e12),
         Letrec (str21, t21, str22, t22, e21, e22)) =>
        str11 = str21 andalso str12 = str22 andalso
        equivalentType(t11, t21) andalso equivalentType (t12, t22) andalso
        equivalent(e11, e21) andalso equivalent(e12, e22)
      | (Prim1(str1, e1), Prim1(str2, e2)) =>
        str1 = str2 andalso equivalent(e1, e2)
      | (Prim2(str1, e11, e12), Prim2(str2, e21, e22)) =>
        str1 = str2 andalso equivalent(e11, e21) andalso equivalent(e12, e22)
      | (If(e11, e12, e13), If(e21, e22, e23)) =>
        equivalent(e11, e21) andalso equivalent (e12, e22) andalso
        equivalent(e13, e23)
      (* Begin match *)
      | (Match(e11, (SOME(e12), e13)::t1), Match(e21, (SOME(e22), e23)::t2)) =>
        equivalent(e11, e21) andalso equivalent (e12, e22) andalso
        equivalent(e13, e23) andalso equivalent (Match(e11, t1), Match(e21, t2))
      | (Match(_, (NONE, _)::_), Match(_, (SOME(e22), _)::_)) => false
      | (Match(_, (SOME(e12), _)::_), Match(_, (NONE, _)::_)) => false
      | (Match(e11, (NONE, e12)::t1), Match(e21, (NONE, e22)::t2)) =>
        equivalent(e11, e21) andalso equivalent (e12, e22) andalso
        equivalent (Match(e11, t1), Match(e21, t2))
      | (Match(e11, []), Match(e21, (e22, e23)::t2)) => false
      | (Match(e11, (e12, e13)::t1), Match(e21, [])) => false
      | (Match(e1, []), Match(e2, [])) => equivalent(e1, e2)
      (* End match *)
      | (Call (e11, e12), Call (e21, e22)) =>
        equivalent (e11, e21) andalso equivalent (e12, e22)
      | (List (h1::t1), List (h2::t2)) =>
        equivalent (h1, h2) andalso equivalent (List t1, List t2)
      | (List [], List (_::_)) => false
      | (List (_::_), List []) => false
      | (List [], List []) => true
      | (Item (i1, e1), Item (i2, e2)) =>
        i1 = i2 andalso equivalent (e1, e2)
      | (Anon (t1, str1, e1), Anon (t2, str2, e2)) =>
        equivalentType (t1, t2) andalso str1 = str2 andalso
        equivalent (e1, e2)
      | _ => false;

fun printTestError (str) =
    (TextIO.output(TextIO.stdOut, "Error in some" ^
                                  " test in case***" ^ str ^ "***\n"); 1)

fun test ((str: string, e: expr)::test_list) =
    if equivalent (fromString str, e)
    then
        test test_list
    else printTestError str
  | test [] = 0

val cases =
  (
    let val s = "0";
        val e = ConI 0
    in
        (s, e)
    end
  ) ::
  (
    let val s = "5+3*4";
        val e = Prim2 ("+",ConI 5,Prim2 ("*",ConI 3,ConI 4))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "-3 < 4";
        val e = Prim2 ("<",Prim1 ("-",ConI 3),ConI 4)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "!(3 = 4)";
        val e = Prim1 ("!",Prim2 ("=",ConI 3,ConI 4))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "3+1 = 4 && 4 <= 3";
        val e = Prim2 ("&&",Prim2 ("=",Prim2 ("+",ConI 3,ConI 1),ConI 4), Prim2 ("<=",ConI 4,ConI 3))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "if 3 = 2 then 0 else 1 + 4";
        val e = If (Prim2 ("=",ConI 3,ConI 2),ConI 0,Prim2 ("+",ConI 1,ConI 4))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "3 + if 3 = 2 then 0 else 1";
        val e = Prim2 ("+",ConI 3,If (Prim2 ("=",ConI 3,ConI 2),ConI 0,ConI 1))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "match x with | 0 -> 1 | _ -> -1 end";
        val e = Match (Var "x",[(SOME (ConI 0), ConI 1), (NONE, Prim1 ("-",ConI 1))])
    in
        (s, e)
    end
  ) ::
  (
    let val s = "4; true";
        val e = Prim2 (";",ConI 4,ConB true)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "4 * (true; 6)";
        val e = Prim2 ("*",ConI 4,Prim2 (";",ConB true,ConI 6))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "( )";
        val e = List []
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,false,())";
        val e = List [ConI 1, ConB false, List []]
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,(2,3),4)";
        val e = List [ConI 1, List [ConI 2, ConI 3], ConI 4]
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(true,false)[1]";
        val e = Item (1, List [ConB true, ConB false])
    in
        (s, e)
    end
  ) ::
  (
    let val s = "((5,6),false)[1][2]";
        val e = Item (2, Item (1, List [List [ConI 5, ConI 6], ConB false]))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 + {3}";
        val e = Prim2 ("+",ConI 1,ConI 3)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "print false";
        val e = Prim1 ("print",ConB false)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "print (1 - 3)";
        val e = Prim1 ("print",Prim2 ("-",ConI 1,ConI 3))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int] [])";
        val e = ESeq (SeqT IntT)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int] [])";
        val e = ESeq (SeqT IntT)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Bool] [])";
        val e = ESeq (SeqT BoolT)
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Nil] [])";
        val e = ESeq (SeqT (ListT []))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([[Int]] [])";
        val e = ESeq (SeqT (SeqT IntT))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int -> Nil] [])";
        val e = ESeq (SeqT (FunT (IntT, ListT [])))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([[Int -> Int -> Bool]] [])";
        val e = ESeq (SeqT (SeqT (FunT (IntT, FunT (IntT,BoolT)))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([(Nil, Int, Bool)] [])";
        val e = ESeq (SeqT (ListT [ListT [], IntT, BoolT]))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 :: ([Int] [])";
        val e = Prim2 ("::",ConI 1,ESeq (SeqT IntT))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 :: 2 :: ([Int] [])";
        val e = Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,ESeq (SeqT IntT)))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,2) :: (3,4) :: ([(Int,Int)] [])";
        val e = Prim2 ("::",
                       List [ConI 1, ConI 2],
                       Prim2 ("::",
                              List [ConI 3, ConI 4],
                              ESeq (SeqT (ListT [IntT, IntT]))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "hd (1 :: 2 :: ([Int] []))";
        val e = Prim1 ("hd",Prim2 ("::",
                                   ConI 1,
                                   Prim2 ("::",ConI 2,ESeq (SeqT IntT))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "tl (1 :: 2 :: ([Int] []))";
        val e = Prim1 ("tl",
                       Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,ESeq (SeqT IntT))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "ise([Int] [])";
        val e = Prim1 ("ise",ESeq (SeqT IntT))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "ise(true::([Bool] []))";
        val e = Prim1 ("ise",Prim2 ("::",ConB true,ESeq (SeqT BoolT)))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; x+1";
    val e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "{var x = 4; x+1}";
        val e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; var y = 6; x + y";
        val e = Let ("x",ConI 4,Let ("y",ConI 6,Prim2 ("+",Var "x",Var "y")))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; print x; {var y = 6; print y }";
        val e = Let ("x",ConI 4,Prim2 (";",Prim1 ("print",Var "x"),Let ("y",ConI 6,Prim1 ("print",Var "y"))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 + {var tmp = 9; x + x}";
        val e = Prim2 ("+",ConI 1,Let ("tmp",ConI 9,Prim2 ("+",Var "x",Var "x")))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var a = (3,4); a[1] < a[2]";
        val e = Let ("a",List [ConI 3, ConI 4],Prim2 ("<",Item (1, Var "a"),Item (2, Var "a")))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var e = ([Bool] []); true::false::e";
        val e = Let ("e",ESeq (SeqT BoolT),Prim2 ("::",ConB true,Prim2 ("::",ConB false,Var "e")))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fn (Int x) => x end";
        val e = Anon (IntT, "x", Var "x")
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f";
        val e = Let ("f",Anon (IntT,"x",Var "x"),Var "f")
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f";
        val e = Let ("f",Anon (IntT, "x",Var "x"),Var "f")
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f(10)";
        val e = Let ("f",Anon (IntT,"x",Var "x"),Call (Var "f",ConI 10))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = x; f";
        val e = Let ("f",Anon (IntT, "x",Var "x"),Var "f")
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = {fun g(Int y) = x+y; g}; f(3)(4)";
        val e = Let ("f", Anon (IntT, "x",Let ("g",Anon (IntT,"y",Prim2 ("+",Var "x",Var "y")),Var "g")),Call (Call (Var "f",ConI 3),ConI 4))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = fn (Int y) => x+y end; f(3)(4)";
        val e = Let ("f",Anon (IntT,"x",Anon (IntT,"y",Prim2 ("+",Var "x",Var "y"))), Call (Call (Var "f",ConI 3),ConI 4))
    in
        (s, e)
    end
  ) ::
  (
    let val s = (
    "fun f (Int -> Bool g) = if g(1) then 10 else 11;"
    ^ "fun h (Int x) = 0 < x;"
    ^ "f(h)");
        val e = Let ("f",Anon (FunT (IntT,BoolT), "g",If (Call (Var "g",ConI 1),ConI 10,ConI 11)), Let ("h",Anon (IntT,"x",Prim2 ("<",ConI 0,Var "x")),Call (Var "f",Var "h")))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(5)";
        val e = Letrec ("f",IntT,"x", IntT, If (Prim2 ("<=",Var "x",ConI 0),ConI 1, Prim2 ("+",Var "x",Call (Var "f",Prim2 ("-",Var "x",ConI 1)))), Call (Var "f",ConI 5))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec pr(Int x): Nil = if x <= 0 then print(0) else { print(x); pr(x-1) }; pr(5)";
        val e = Letrec ("pr",IntT,"x", ListT [], If (Prim2 ("<=",Var "x",ConI 0),Prim1 ("print",ConI 0), Prim2 (";",Prim1 ("print",Var "x"),Call (Var "pr",Prim2 ("-",Var "x",ConI 1)))),Call (Var "pr",ConI 5))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec len([Int] l): Int = if ise(l) then 0 else 1 + len(tl(l)); len(1::2::([Int] []))";
        val e = Letrec ("len",SeqT IntT,"l", IntT, If (Prim1 ("ise",Var "l"),ConI 0, Prim2 ("+",ConI 1,Call (Var "len",Prim1 ("tl",Var "l")))), Call (Var "len",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,ESeq (SeqT IntT)))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fn (Int x, Int y) => x - y end";
        val e = Anon (ListT [IntT, IntT], "$list", Let ("x",Item (1, Var "$list"), Let ("y",Item (2, Var "$list"),Prim2 ("-",Var "x",Var "y"))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Int y) = x - y; f(5,4)";
        val e = Let ("f", Anon (ListT [IntT, IntT], "$list", Let ("x",Item (1, Var "$list"), Let ("y",Item (2, Var "$list"),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",List [ConI 5, ConI 4]))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var p = (1,3); fun f(Int x, Int y) = x - y; f(p)";
        val e = Let ("p",List [ConI 1, ConI 3], Let ("f", Anon (ListT [IntT, IntT], "$list", Let ("x",Item (1, Var "$list"), Let ("y",Item (2, Var "$list"),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",Var "p")))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Int y, Int z) = x - y * z ; f(5,4,2)";
        val e = Let ("f", Anon (ListT [IntT, IntT, IntT], "$list", Let ("x",Item (1, Var "$list"), Let ("y",Item (2, Var "$list"), Let ("z",Item (3, Var "$list"), Prim2 ("-",Var "x",Prim2 ("*",Var "y",Var "z")))))), Call (Var "f",List [ConI 5, ConI 4, ConI 2]))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec mem(Int x, [Int] l): Bool = if ise(l) then false else if x = hd(l) then true else mem(x, tl(l)); mem(2, 1::2::([Int] []))";
        val e = Letrec ("mem",ListT [IntT, SeqT IntT], "$list", BoolT, Let ("x",Item (1, Var "$list"), Let ("l",Item (2, Var "$list"),If (Prim1 ("ise",Var "l"),ConB false,If (Prim2 ("=",Var "x",Prim1 ("hd",Var "l")),ConB true, Call (Var "mem",List [Var "x", Prim1 ("tl",Var "l")]))))),Call (Var "mem", List [ConI 2, Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,ESeq (SeqT IntT)))]))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun inc (Int x) = x + 1; fun add (Int x, Int y) = x + y; fun cadd (Int x) = fn (Int y) => x + y end; var y = add(3, inc(4)); var x = cadd(3)(7-y); var z = x * 3; fun rec fac (Int n) : Int = match n with | 0 -> 1 | 1 -> 1 | _ -> n * fac(n - 1) end; print x; print y; x :: y :: z :: fac(z) :: ([Int] [])";
        val e = Let("inc",Anon (IntT,"x",Prim2 ("+",Var "x",ConI 1)), Let ("add",Anon(ListT [IntT, IntT],"$list", Let ("x",Item (1,Var "$list"),Let ("y",Item (2,Var "$list"),Prim2 ("+",Var "x",Var "y")))),Let("cadd",Anon (IntT,"x",Anon (IntT,"y",Prim2 ("+",Var "x",Var "y"))), Let ("y",Call (Var "add",List [ConI 3, Call (Var "inc",ConI 4)]),Let("x", Call (Call (Var "cadd",ConI 3),Prim2 ("-",ConI 7,Var "y")), Let ("z",Prim2 ("*",Var "x",ConI 3),Letrec("fac",IntT,"n",IntT, Match (Var "n",[(SOME (ConI 0), ConI 1), (SOME (ConI 1), ConI 1), (NONE,Prim2("*",Var "n", Call (Var "fac",Prim2 ("-",Var "n",ConI 1))))]), Prim2 (";",Prim1 ("print",Var "x"),Prim2(";",Prim1 ("print",Var "y"), Prim2 ("::",Var "x",Prim2("::",Var "y", Prim2 ("::",Var "z",Prim2("::",Call (Var "fac",Var "z"), ESeq (SeqT IntT))))))))))))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Bool b) = match b with | true -> {x + 1} | _    -> x end; f(3,true)";
        val e = Let("f", Anon (ListT [IntT, BoolT],"$list",Let("x",Item (1,Var "$list"), Let ("b",Item (2,Var "$list"),Match(Var "b", [(SOME (ConB true), Prim2 ("+",Var "x",ConI 1)),(NONE, Var "x")])))),Call (Var "f",List [ConI 3, ConB true]))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var E = ([Int] []); fun reverse ([Int] l) = { fun rec rev ([Int] l1, [Int] l2): [Int] = if ise(l1) then l2 else rev(tl(l1), hd(l1)::l2); rev(l, E) }; reverse (1::2::3::E)";
        val e = Let ("E",ESeq (SeqT IntT),Let ("reverse",Anon (SeqT IntT, "l",Letrec ("rev", ListT [SeqT IntT, SeqT IntT],"$list", SeqT IntT, Let ("l1",Item (1, Var "$list"),Let ("l2",Item (2, Var "$list"), If (Prim1 ("ise",Var "l1"),Var "l2",Call (Var "rev",List [Prim1 ("tl",Var "l1"),Prim2 ("::",Prim1 ("hd",Var "l1"),Var "l2")])))),Call (Var "rev",List [Var "l", Var "E"]))),Call (Var "reverse",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,Prim2 ("::",ConI 3,Var "E"))))))
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var E = ([Int] []); fun reverse ([Int] s) = { fun rec rev ([Int] s1, [Int] s2): [Int] = match s1 with | E -> s2 | _ -> { var h = hd(s1); var t = tl(s1); rev(t, h::s2) } end; rev(s, E) }; reverse (1::2::3::E)";
        val e = Let("E",ESeq (SeqT IntT), Let ("reverse",Anon(SeqT IntT,"s",Letrec("rev",ListT [SeqT IntT, SeqT IntT],"$list",SeqT IntT, Let ("s1",Item (1,Var "$list"),Let("s2",Item (2,Var "$list"), Match (Var "s1",[(SOME (Var "E"), Var "s2"), (NONE,Let("h",Prim1 ("hd",Var "s1"), Let ("t",Prim1 ("tl",Var "s1"),Call(Var "rev", List [Var "t", Prim2 ("::",Var "h",Var "s2")]))))]))), Call (Var "rev",List [Var "s", Var "E"]))),Call (Var "reverse", Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,Prim2 ("::",ConI 3,Var "E"))))))
    in
        (s, e)
    end
  ) ::
  [ (
    let val s =
    "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";
        val e = Letrec ("map",FunT (IntT,IntT), "f", FunT (SeqT IntT,SeqT IntT), Anon(SeqT IntT, "l",If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),Call(Call (Var "map",Anon (IntT, "x",Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,ESeq (SeqT IntT))))))
    in
        (s, e)
    end
  ) ];
