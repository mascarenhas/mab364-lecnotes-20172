
package object fun {
  trait Valor <: Exp

  trait Exp {
    def +(e: Exp) = Soma(this, e)
    def *(e: Exp) = Mult(this, e)
    def -(e: Exp) = Soma(this, Neg(e))
    def <(e: Exp) = Menor(this, e)
  }
  case class Num(v: String) extends Valor
  case class Soma(e1: Exp, e2: Exp) extends Exp
  case class Mult(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp

  def Neg(e: Exp): Exp = Mult(Num("-1"), e)

  case class Bool(v: Boolean) extends Valor
  case class Menor(e1: Exp, e2: Exp) extends Exp
  case class If(ec: Exp, et: Exp, ee: Exp) extends Exp

  case class Erro(msg: String) extends Valor

  case class Fun1(params: List[String], corpo: Exp)
  case class Prog(defs: Map[String, Fun1], corpo: Exp)
  case class Var(nome: String) extends Exp
  case class Ap1(fun: String, args: List[Exp]) extends Exp

  def subst(smap: Map[String, Exp], e: Exp): Exp = e match {
    case Soma(e1, e2) => Soma(subst(smap, e1),
      subst(smap, e2))
    case Mult(e1, e2) => Mult(subst(smap, e1),
      subst(smap, e2))
    case Div(e1, e2) => Div(subst(smap, e1),
      subst(smap, e2))
    case Menor(e1, e2) => Menor(subst(smap, e1),
      subst(smap, e2))
    case If(ec, et, ee) => If(subst(smap, ec),
      subst(smap, et), subst(smap, ee))
    case Ap1(fun, args) => Ap1(fun,
      args.map(arg => subst(smap, arg)))
    case Var(nome) => smap.get(nome) match {
      case Some(v) => v
      case None => e
    }
    case _ => e
  }

  def run(prog: Prog) = {
    val Prog(defs, corpo) = prog

    def eval(e: Exp): Valor = e match {
      case Erro(e) => Erro(e)
      case Num(v) => Num(v)
      case Bool(v) => Bool(v)
      case Soma(e1, e2) => (eval(e1), eval(e2)) match {
        case (Num(n1), Num(n2)) => Num((BigDecimal(n1) +
          BigDecimal(n2)).toString())
        case (Erro(msg), _) => Erro(msg)
        case (_, Erro(msg)) => Erro(msg)
        case _ => Erro("aritmética precisa de números")
      }
      case Mult(e1, e2) => (eval(e1), eval(e2)) match {
        case (Num(n1), Num(n2)) => Num((BigDecimal(n1) *
          BigDecimal(n2)).toString())
        case (Erro(msg), _) => Erro(msg)
        case (_, Erro(msg)) => Erro(msg)
        case _ => Erro("aritmética precisa de números")
      }
      case Div(e1, e2) => (eval(e1), eval(e2)) match {
        case (Num(n1), Num(n2)) => Num((BigDecimal(n1) /
          BigDecimal(n2)).toString())
        case (Erro(msg), _) => Erro(msg)
        case (_, Erro(msg)) => Erro(msg)
        case _ => Erro("aritmética precisa de números")
      }
      case Menor(e1, e2) => (eval(e1), eval(e2)) match {
        case (Num(n1), Num(n2)) => Bool(BigDecimal(n1) <
          BigDecimal(n2))
        case (Erro(msg), _) => Erro(msg)
        case (_, Erro(msg)) => Erro(msg)
        case _ => Erro("comparação precisa de números")
      }
      case If(ec, et, ee) => eval(ec) match {
        case Bool(v) => if (v) eval(et) else eval(ee)
        case Erro(msg) => Erro(msg)
        case _ => Erro("if precisa de um booleano")
      }
      case Var(nome) => Erro(
        "parâmetro " + nome + " não declarado")
      case Ap1(fun, args) => defs.get(fun) match {
        case Some(Fun1(params, corpo)) => {
          if(params.size == args.size) {
            val eargs: List[Valor] = args.map(arg => eval(arg))
            val smap: Map[String, Valor] =
              params.zip(eargs).toMap
            val scorpo = subst(smap, corpo)
            eval(scorpo)
          } else Erro("aridade incompatível chamando " + fun)
        }
        case None => Erro("funcão " + fun + " não existe")
      }
    }

    eval(corpo)
  }

  def runs(prog: Prog) = {
    val Prog(defs, corpo) = prog

    def step(e: Exp): Exp = e match {
      case Soma(Num(v1), Num(v2)) =>
        Num((BigDecimal(v1) + BigDecimal(v2)).toString())
      case Soma(Num(v), _: Valor) =>
        Erro("aritmética precisa de números")
      case Soma(Num(v), d) =>
        Soma(Num(v), step(d))
      case Soma(_: Valor, d) =>
        Erro("aritmética precisa de números")
      case Soma(e, d) => Soma(step(e), d)

      case Mult(Num(v1), Num(v2)) =>
        Num((BigDecimal(v1) * BigDecimal(v2)).toString())
      case Mult(Num(_), _: Valor) =>
        Erro("aritmética precisa de números")
      case Mult(Num(v), d) =>
        Mult(Num(v), step(d))
      case Mult(_: Valor, d) =>
        Erro("aritmética precisa de números")
      case Mult(e, d) => Mult(step(e), d)

      case Div(Num(v1), Num(v2)) =>
        Num((BigDecimal(v1) / BigDecimal(v2)).toString())
      case Div(Num(_), _: Valor) =>
        Erro("aritmética precisa de números")
      case Div(Num(v), d) =>
        Div(Num(v), step(d))
      case Div(_: Valor, d) =>
        Erro("aritmética precisa de números")
      case Div(e, d) => Div(step(e), d)

      case Menor(Num(v1), Num(v2)) =>
        Bool(BigDecimal(v1) < BigDecimal(v2))
      case Menor(Num(_), _: Valor) =>
        Erro("comparação precisa de números")
      case Menor(Num(v), d) =>
        Menor(Num(v), step(d))
      case Menor(_: Valor, d) =>
        Erro("comparação precisa de números")
      case Menor(e, d) => Menor(step(e), d)

      case If(Bool(true), et, ee) => et
      case If(Bool(false), et, ee) => ee
      case If(_: Valor, et, ee) =>
        Erro("if precisa de um booleano")
      case If(ec, et, ee) => If(step(ec), et, ee)
    }

    def stepn(e: Exp): Valor = e match {
      case Num(v) => Num(v)
      case Bool(v) => Bool(v)
      case Erro(v) => Erro(v)
      case _ => stepn(step(e))
    }

    stepn(corpo)
  }

}