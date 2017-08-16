import fun._

val e1: Exp = Soma(Num("2"), Mult(Num("2"), Num("3")))

val e2: Exp = Num("2") ++ Num("2") ** Num("3")

eval(e1)
eval(e2)
