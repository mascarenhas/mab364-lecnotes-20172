import parser._

package object dyn {

  trait Exp
  case class Num(n: Int) extends Exp
  case class Soma(e1: Exp, e2: Exp) extends Exp
  case class Mult(e1: Exp, e2: Exp) extends Exp
  // Testa se a condição é igual a 0
  case class If(cond: Exp, ethen: Exp, eelse: Exp) extends Exp
  // Funções têm apenas um parâmetro
  case class Fun(param: String, corpo: Exp) extends Exp
  case class Var(nome: String) extends Exp
  case class Ap(fun: Exp, arg: Exp) extends Exp
  case class Let(nome: String, exp: Exp, corpo: Exp) extends Exp
  case class LetDyn(nome: String, exp: Exp, corpo: Exp) extends Exp

  trait Valor

  type Env = Map[String, Valor]

  def parse(s: String): Exp = {

  def ProgFun: Parser[Exp] = for {
    corpo <- ExpFun
    _ <- space
    _ <- not(pred(c => true), ())
  } yield corpo

  def ExpFun: Parser[Exp] =
    chainl(FatorFun,
      (for { _ <- op("+") } yield (e1, e2) => Soma(e1, e2)) +:
        (for { _ <- op("-") } yield (e1: Exp, e2: Exp) =>
          Soma(e1, Mult(Num(-1), e2))),
      FatorFun)

  def FatorFun: Parser[Exp] =
    (for {
      _ <- kw("fun")
      _ <- op("(")
      (param, _) <- id
      _ <- op(")")
      corpo <- ExpFun
      _ <- kw("end")
    } yield Fun(param, corpo)) +:
      (for { (v, pos) <- num } yield Num(v.toInt)) +:
      (for {
        _ <- op("(")
        e <- ExpFun
        _ <- op(")")
        _ <- op("(")
        a <- ExpFun
        _ <- op(")")
        resto <- many(for{
          _ <- op("(")
          a <- ExpFun
          _ <- op(")")
        } yield a)
      } yield (a :: resto).foldLeft(e)((f, a) => Ap(f, a))) +:
      (for {
        _ <- op("(")
        e <- ExpFun
        _ <- op(")")
      } yield e) +:
      (for {
        _ <- kw("if")
        econd <- ExpFun
        _ <- kw("then")
        ethen <- ExpFun
        _ <- kw("else")
        eelse <- ExpFun
        _ <- kw("end")
      } yield If(econd, ethen, eelse)) +:
      (for {
        _ <- kw("let")
        (nome, _) <- id
        _ <- op("=")
        exp <- ExpFun
        resto <- many(for {
          _ <- op(",")
          (nome, _) <- id
          _ <- op("=")
          exp <- ExpFun
        } yield (nome, exp))
        _ <- kw("in")
        corpo <- ExpFun
        _ <- kw("end")
      } yield ((nome, exp) :: resto).foldRight(corpo){
        case ((nome, exp), corpo) => Let(nome, exp, corpo)
      }) +:
      (for {
        _ <- kw("letdyn")
        (nome, _) <- id
        _ <- op("=")
        exp <- ExpFun
        resto <- many(for {
          _ <- op(",")
          (nome, _) <- id
          _ <- op("=")
          exp <- ExpFun
        } yield (nome, exp))
        _ <- kw("in")
        corpo <- ExpFun
        _ <- kw("end")
      } yield ((nome, exp) :: resto).foldRight(corpo){
        case ((nome, exp), corpo) => LetDyn(nome, exp, corpo)
      }) +:
      (for { (nome, _) <- id } yield Var(nome))

    def dyn: Parser[Exp] = for {
      e <- ProgFun
      _ <- space
      _ <- not(pred(c => true), ())
    } yield e

    dyn.parse(s)
  }

  def eval(envd: Env, envs: Env, e: Exp) = ???

  def run(e: Exp) = eval(Map(), Map(), e)

}
