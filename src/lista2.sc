import lista2._

val cl1 = Abs("x", Var("y"))
val cl2 = Abs("z", Var("y"))

val cl_true = Abs("x",
  Abs("y", Var("x")))
val cl_false = Abs("x",
  Abs("y", Var("y")))


val cl_zero = Abs("x",
  Abs("y", Var("y")))
val cl_um = Abs("x",
  Abs("y", Ap(Var("x"), Var("y"))))
val cl_dois = Abs("x",
  Abs("y", Ap(Var("x"), Ap(Var("x"), Var("y")))))

/*
x y => x(y)
x y z => x(y(z))

\n \x \y x ((n x) y)

fun suc(n)
  fun (x)
    fun (y)
      x(n(x)(y))
    end
  end
end
 */

val cl_suc = Abs("n",
  Abs("x", Abs("y",
    Ap(Var("x"), Ap(Ap(Var("n"),
      Var("x")), Var("y"))))))

subst("y", cl1, cl2)

eval(Ap(cl_suc, cl_dois))
