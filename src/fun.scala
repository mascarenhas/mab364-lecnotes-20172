package object fun {
  trait Exp {
    def ++(e: Exp) = Soma(this, e)
    def **(e: Exp) = Mult(this, e)
  }
  case class Num(v: String) extends Exp
  case class Soma(e1: Exp, e2: Exp) extends Exp
  case class Mult(e1: Exp, e2: Exp) extends Exp

  def eval(e: Exp): Double = e match {
    case Num(v) => v.toDouble
    case Soma(e1, e2) => eval(e1) + eval(e2)
    case Mult(e1, e2) => eval(e1) * eval(e2)
  }
}