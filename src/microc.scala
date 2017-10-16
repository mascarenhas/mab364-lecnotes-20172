import parser._

package object microc {
  type End = Int
  type Mem = Map[End, Valor]
  type Env = Map[String, End]

  type Valor = Int

  trait Exp

  case class Num(v: String) extends Exp
  case class Soma(e1: Exp, e2: Exp) extends Exp
  case class Mult(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp
  def Neg(e: Exp): Exp = Mult(Num("-1"), e)
  case class Menor(e1: Exp, e2: Exp) extends Exp
  case class If(ec: Exp, et: Exp, ee: Exp) extends Exp

  case class Fun(params: List[String], corpo: Exp)
  case class Prog(defs: Map[String, Fun], corpo: Exp)

  case class Var(nome: String) extends Exp
  case class Ap(fun: String, args: List[Exp]) extends Exp

  case class Let(nome: String, exp: Exp, corpo: Exp) extends Exp
  case class Seq(e1: Exp, e2: Exp) extends Exp
  case class AtribVar(lval: String, rval: Exp) extends Exp
  case class AtribPont(lval: Deref, rval: Exp) extends Exp

  case class Deref(exp: Exp) extends Exp
  case class Ender(nome: String) extends Exp

  def parse(s: String): Prog = {
    def ProgFun: Parser[Prog] = for {
      funs <- many(DefFun)
      corpo <- ExpFun
    } yield Prog(funs.toMap, corpo)

    // fun id '(' [id {, id}] ')' exp end
    def DefFun: Parser[(String, Fun)] = for {
      _ <- kw("fun")
      (nome, _) <- id
      _ <- op("(")
      params <- (for {
        (p, _) <- id
        ps <- many(for {
          _ <- op(",")
          (p, _) <- id
        } yield p)
      } yield p :: ps) +: empty(List())
      _ <- op(")")
      corpo <- ExpFun
      _ <- kw("end")
    } yield (nome, Fun(params, corpo))

    // exp -> aexp {< aexp}
    def ExpFun: Parser[Exp] =
      chainl(AtribFun,
        for { _ <- op(";") } yield (e1: Exp, e2: Exp) => Seq(e1, e2),
        AtribFun)

    def AtribFun: Parser[Exp] = (for {
      (n, _) <- id
      _ <- op(":=")
      e <- RelFun
    } yield AtribVar(n, e)) +: (for {
      _ <- op("*")
      e1 <- FatorFun
      _ <- op(":=")
      e2 <- RelFun
    } yield AtribPont(Deref(e1), e2)) +: RelFun

    def RelFun: Parser[Exp] =
      chainl(AexpFun,
        for { _ <- op("<") } yield (e1: Exp, e2: Exp) => Menor(e1, e2),
        AexpFun)

    // aexp -> termo {+ termo|-termo}
    def AexpFun: Parser[Exp] =
      chainl(TermoFun,
        (for { _ <- op("+") } yield (e1: Exp, e2: Exp) => Soma(e1, e2)) +:
          (for { _ <- op("-") } yield (e1: Exp, e2: Exp) =>
            Soma(e1, Mult(Num("-1"), e2))),
        TermoFun)

    // termo -> fator {* fator|/ fator}
    def TermoFun: Parser[Exp] =
      chainl(FatorFun, (for {
        _ <- op("*")
      } yield (e1: Exp, e2: Exp) => Mult(e1, e2)) +:
        (for {
          _ <- op("/")
        } yield (e1: Exp, e2: Exp) => Div(e1, e2))
        , FatorFun)

    // fator -> '(' exp ')' | num | id |
    //     if exp then exp else exp end | true | false
    def FatorFun: Parser[Exp] =
        (for { (v, pos) <- num } yield Num(v)) +:
        (for {
          _ <- op("(")
          e <- ExpFun
          _ <- op(")")
        } yield e) +:
        (for {
          _ <- op("-")
          e <- FatorFun
        } yield Mult(Num("-1"), e)) +:
        (for {
          _ <- op("*")
          e <- FatorFun
        } yield Deref(e)) +:
        (for {
          _ <- op("&")
          (n, _) <- id
        } yield Ender(n)) +:
        (for {
          _ <- kw("if")
          econd <- ExpFun
          _ <- kw("then")
          ethen <- ExpFun
          _ <- kw("else")
          eelse <- ExpFun
          _ <- kw("end")
        } yield If(econd, ethen, eelse)) +:
        LetFun +:
        (for {
          (nome, _) <- id
          _ <- op("(")
          args <- (for {
            a <- ExpFun
            as <- many(for {
              _ <- op(",")
              a <- ExpFun
            } yield a)
          } yield a :: as) +: empty(List())
          _ <- op(")")
        } yield Ap(nome, args)) +:
        (for { (n, pos) <- id } yield Var(n))

    def LetFun: Parser[Exp] = for {
      _ <- kw("let")
      es <- (for {
        (n1, pos) <- id
        _ <- op("=")
        e1 <- ExpFun
        es <- many(for {
          _ <- op(",")
          (n, pos) <- id
          _ <- op("=")
          e <- ExpFun
        } yield (n, e))
      } yield (n1, e1) :: es)
      _ <- kw("in")
      ce <- ExpFun
      _ <- kw("end")
    } yield es.foldRight(ce)({ case ((n, e), c) => Let(n, e, c) })

    def microc: Parser[Prog] = for {
      e <- ProgFun
      _ <- space
      _ <- not(pred(c => true), ())
    } yield e

    microc.parse(s)
  }

  // Ações primitivas
  type Acao[+T] = (End, Mem) => (T, End, Mem)

  implicit class RichAcao[+T](val a: Acao[T]) extends AnyVal {
    def flatMap[U](f: T => Acao[U]): Acao[U] = bind(a, f)
    def map[U](f: T => U): Acao[U] = bind(a, (v: T) => lift(f(v)))
    def withFilter(p: T => Boolean): Acao[T] = a
  }

  def lift[T](v: T): Acao[T] = (sp, mem) => (v, sp, mem)

  def le(l: End): Acao[Valor] = (sp, mem) => (mem.getOrElse(l, 0), sp, mem)

  def escreve(l: End, v: Valor): Acao[Valor] =
    (sp, mem) => (v, sp, mem + (l -> v))

  def bind[T, U](a: Acao[T], f: T => Acao[U]): Acao[U] =
    (sp, mem) => {
      val (v, nsp, nmem) = a(sp, mem)
      val b: Acao[U] = f(v)
      b(nsp, nmem)
    }

  def push(v: Valor): Acao[End] = (sp, mem) => (sp, sp+1, mem + (sp -> v))

  def pop(n: Int): Acao[Unit] = (sp, mem) => ((), sp - n, mem)

  def foldAcao[T](l: List[Acao[T]]): Acao[List[T]] = l match {
    case List() => lift(List())
    case a :: as => for {
      v <- a
      vs <- foldAcao(as)
    } yield v :: vs
  }

  def run(prog: Prog) = {
    val Prog(defs, corpo) = prog

    def eval(env: Env, e: Exp): Acao[Valor] = e match {
      case Num(v) => lift(v.toInt)
      case Soma(e1, e2) => for {
        n1 <- eval(env, e1)
        n2 <- eval(env, e2)
      } yield n1 + n2
      case Mult(e1, e2) => for {
        n1 <- eval(env, e1)
        n2 <- eval(env, e2)
      } yield n1 * n2
      case Div(e1, e2) => for {
        n1 <- eval(env, e1)
        n2 <- eval(env, e2)
      } yield n1 / n2
      case Menor(e1, e2) => for {
        n1 <- eval(env, e1)
        n2 <- eval(env, e2)
      } yield if (n1 < n2) 1 else 0
      case If(c, et, ee) => for {
        n <- eval(env, c)
        v <- if (n == 0) eval(env, ee) else eval(env, et)
      } yield v
      case Var(nome) => le(env(nome)) // dereferência implícita
      case Let(n, e, c) => for {
        ve <- eval(env, e)
        l <- push(ve)
        vl <- eval(env + (n -> l), c)
        _ <- pop(1)
      } yield vl
      case Seq(e1, e2) => for {
        _ <- eval(env, e1)
        v <- eval(env, e2)
      } yield v
      case AtribVar(lv, rv) => for {
        v <- eval(env, rv)
        _ <- escreve(env(lv), v)
      } yield v
      case Deref(e) => for {
        vp <- eval(env, e)
        v <- le(vp)
      } yield v
      case Ender(n) => lift(env(n))
      case AtribPont(Deref(e), rv) => for {
        vp <- eval(env, e)
        vr <- eval(env, rv)
        _ <- escreve(vp, vr)
      } yield vr
      case Ap(f, as) => {
        val Fun(ps, c) = defs(f)
        for {
          vas <- foldAcao(as.map(a => eval(env, a)))
          ls <- foldAcao(vas.map(va => push(va)))
          v <- eval(ps.zip(ls).toMap, c)
          _ <- pop(vas.length)
        } yield v
      }
    }

    eval(Map(), corpo)(1000, Map(0 -> 1))
  }
}