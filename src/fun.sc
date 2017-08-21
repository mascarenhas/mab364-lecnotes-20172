import fun._

val e1: Exp = Soma(Num("2"), Mult(Num("2"), Num("3")))

val e2: Exp = Num("2.00000000000000001") ++ Num("2") ** Num("3")

val e3: Exp = Num("0.1") ++ Num("0.2")

val e4: Exp = Num("3") -- Num("2")

eval(e1)
eval(e2)
eval(e3)
eval(e4)

stepn(e1)
stepn(e2)
stepn(e3)
stepn(e4)

BigDecimal("0.1") + BigDecimal("0.2")

0.1 + 0.2

eval(If(Num("2"), Num("3"), Num("4")))
