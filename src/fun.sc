import fun._

val e1: Exp = Soma(Num("2"), Mult(Num("2"), Num("3")))

val e2: Exp = Num("2.00000000000000001") + Num("2") * Num("3")

val e3: Exp = Num("0.1") + Num("0.2")

val e4: Exp = Num("3") - Num("2")

run(Prog(Map(), e1))
run(Prog(Map(), e2))
run(Prog(Map(), e3))
run(Prog(Map(), e4))

runs(Prog(Map(), e1))
runs(Prog(Map(), e2))
runs(Prog(Map(), e3))
runs(Prog(Map(), e4))

BigDecimal("0.1") + BigDecimal("0.2")

0.1 + 0.2

run(Prog(Map(), Num("2") + Bool(true) + Num("3")))

run(Prog(Map(), If(If(Num("2"), Num("1"), Num("0")),
  Num("1"),Num("0"))))

runs(Prog(Map(), Num("2") + Bool(true) + Num("3")))

runs(Prog(Map(), If(If(Num("2"), Num("1"), Num("0")),
  Num("1"),Num("0"))))

val fat = Fun1(
  List("n"),
    If(Var("n") < Num("2"),
      Num("1"),
      Var("n") *
        Ap1("fat",
          List(Var("n") -
            Num("1"))
        ))
)

run(Prog(Map("fat" -> fat),
  Ap1("fat", List(Num("2") + Num("3")))
))

run(Prog(Map("fat" -> fat),
  Ap1("F", List(Num("2") + Num("3")))
))

val faterr = Fun1(
  List("n"),
  If(Var("n") < Num("2"),
    Num("1"),
    Var("n") *
      Ap1("fat",
        List(Var("x") -
          Num("1"))
      ))
)

run(Prog(Map("fat" -> faterr),
  Ap1("fat", List(Num("2") + Num("3")))
))

runs(Prog(Map("fat" -> fat),
  Ap1("fat", List(Num("2") + Num("3")))
))

val fat_tr = Fun1(
  List("n", "a"),
  If(Var("n") < Num("2"),
    Var("a"),
    Ap1("fat_tr", List(
      Var("n")-Num("1"),
      Var("a") * Var("n")
    ))))

runs(Prog(Map("fat_tr" -> fat_tr),
  Ap1("fat_tr", List(Num("2") + Num("3"), Num("1")))
))

val loop = Fun1(List(),
  Ap1("loop", List()))

val f = Fun1(List("_x"), Num("2"))

run(Prog(Map("loop" -> loop,
"f" -> f), Ap1("f", List(Ap1("loop", List())))))

runs(Prog(Map("loop" -> loop,
  "f" -> f), Ap1("f", List(Ap1("loop", List())))))

run(Prog(Map(), Let("x", Num("2")+ Num("3"), Var("x") * Var("x"))))

/*
let x = 2 + 3 in
  let x = 2 + x in
    x * x
  end
end
*/

runs(Prog(Map(), Let("x", Num("2")+ Num("3"),
  Let("x", Num("2") + Var("x"), Var("x") * Var("x")))))

runs(Prog(Map(), Let("_x", Num("2")+ Var("y"),
  Let("y", Num("5"), Var("_x")))))