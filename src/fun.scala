
package object fun {
  trait Valor <: Exp {
    def +(v: Valor) = {
      val Num(n1) = this
      val Num(n2) = v
      Num((BigDecimal(n1) + BigDecimal(n2)).toString())
    }
    def *(v: Valor) = {
      val Num(n1) = this
      val Num(n2) = v
      Num((BigDecimal(n1) * BigDecimal(n2)).toString())
    }
    def /(v: Valor) = {
      val Num(n1) = this
      val Num(n2) = v
      Num((BigDecimal(n1) / BigDecimal(n2)).toString())
    }
    def <(v: Valor) = {
      val Num(n1) = this
      val Num(n2) = v
      Bool(BigDecimal(n1) < BigDecimal(n2))
    }
  }

  trait Exp {
    def ++(e: Exp) = Soma(this, e)
    def **(e: Exp) = Mult(this, e)
    def --(e: Exp) = Soma(this, Neg(e))
    def <<(e: Exp) = Menor(this, e)
  }
  case class Num(v: String) extends Valor
  case class Soma(e1: Exp, e2: Exp) extends Exp
  case class Mult(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp

  def Neg(e: Exp): Exp = Mult(Num("-1"), e)

  case class Bool(v: Boolean) extends Valor
  case class Menor(e1: Exp, e2: Exp) extends Exp
  case class If(ec: Exp, et: Exp, ee: Exp) extends Exp

  def eval(e: Exp): Valor = e match {
    case Num(v) => Num(v)
    case Bool(v) => Bool(v)
    case Soma(e1, e2) => eval(e1) + eval(e2)
    case Mult(e1, e2) => eval(e1) * eval(e2)
    case Div(e1, e2) => eval(e1) / eval(e2)
    case Menor(e1, e2) => eval(e1) < eval(e2)
    case If(ec, et, ee) => {
      val Bool(v) = eval(ec)
      if (v) eval(et) else eval(ee)
    }
  }

  def step(e: Exp): Exp = e match {
    case Soma(Num(v1), Num(v2)) =>
      Num((BigDecimal(v1) + BigDecimal(v2)).toString())
    case Soma(Num(v), d) =>
      Soma(Num(v), step(d))
    case Soma(e, d) => Soma(step(e), d)
    case Mult(Num(v1), Num(v2)) =>
      Num((BigDecimal(v1) * BigDecimal(v2)).toString())
    case Mult(Num(v), d) =>
      Mult(Num(v), step(d))
    case Mult(e, d) => Mult(step(e), d)
    case Div(Num(v1), Num(v2)) =>
      Num((BigDecimal(v1) / BigDecimal(v2)).toString())
    case Div(Num(v), d) =>
      Div(Num(v), step(d))
    case Div(e, d) => Div(step(e), d)
    case Menor(Num(v1), Num(v2)) =>
      Bool(BigDecimal(v1) < BigDecimal(v2))
    case Menor(Num(v), d) =>
      Menor(Num(v), step(d))
    case Menor(e, d) => Menor(step(e), d)
    case If(Bool(true), et, ee) => et
    case If(Bool(false), et, ee) => ee
    case If(ec, et, ee) => If(step(ec), et, ee)
  }

  def stepn(e: Exp): Valor = e match {
    case Num(v) => Num(v)
    case Bool(v) => Bool(v)
    case _ => stepn(step(e))
  }
}