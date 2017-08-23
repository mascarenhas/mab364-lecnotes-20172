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
