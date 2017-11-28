import parser._

package object fun {
  type End = Int
  type Mem = Map[End, Valor]
  type Env = Map[String, Valor]

  trait Valor <: Exp

  trait Exp

  case class Num(v: String) extends Valor
  case class Soma(e1: Exp, e2: Exp) extends Exp
  case class Mult(e1: Exp, e2: Exp) extends Exp
  case class Div(e1: Exp, e2: Exp) extends Exp

  def Neg(e: Exp): Exp = Mult(Num("-1"), e)

  case class Bool(v: Boolean) extends Valor
  case class Menor(e1: Exp, e2: Exp) extends Exp
  case class If(ec: Exp, et: Exp, ee: Exp) extends Exp

  case class Erro(msg: String) extends Valor

  case class Fun(env: Env, params: List[String], corpo: Exp) extends Valor
  case class Proto(params: List[String], corpo: Exp) extends Exp
  case class Prog(defs: Map[String, Proto], corpo: Exp)
  case class Var(nome: String) extends Exp
  case class Ap(fun: Exp, args: List[Exp]) extends Exp

  case class Let(nome: String, exp: Exp, corpo: Exp)
    extends Exp

  case class Rec(nome: String, corpo: Exp) extends Exp

  case class Seq(e1: Exp, e2: Exp) extends Exp
  case class Atrib(lval: Exp, rval: Exp) extends Exp
  case class Ref(e: Exp) extends Exp
  case class Deref(l: Exp) extends Exp

  case class Caixa(l: End) extends Valor

  case class Throw(msg: String) extends Exp
  case class TryCatch(etry: Exp, ecatch: Exp) extends Exp
  case class TryFinally(etry: Exp, efin: Exp) extends Exp
  case class TryCatchFinally(etry: Exp, ecatch: Exp, efin: Exp) extends Exp

  case class Thunk(env: Env, e: Exp) extends Valor
  case class ThunkR(env: Env, n: String, e: Exp) extends Valor

  def parse(s: String): Prog = {
    def ProgFun: Parser[Prog] = for {
      funs <- many(DefFun)
      corpo <- ExpFun
    } yield Prog(funs.toMap, corpo)

    // fun id '(' [id {, id}] ')' exp end
    def DefFun: Parser[(String, Proto)] = for {
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
    } yield (nome, Proto(params, corpo))

    // fun '(' [id {, id}] ')' exp end
    def AnonFun: Parser[Proto] = for {
      _ <- kw("fun")
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
    } yield Proto(params, corpo)

    // exp -> aexp {< aexp}
    def ExpFun: Parser[Exp] =
      chainl(AtribFun,
        for { _ <- op(";") } yield (e1: Exp, e2: Exp) => Seq(e1, e2),
        AtribFun)

    def AtribFun: Parser[Exp] =
      chainl(RelFun,
        for { _ <- op(":=") } yield (e1: Exp, e2: Exp) => Atrib(e1, e2),
        RelFun)

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
          _ <- op("(")
          args <- (for {
            a <- ExpFun
            as <- many(for {
              _ <- op(",")
              a <- ExpFun
            } yield a)
          } yield a :: as) +: empty(List())
          _ <- op(")")
        } yield Ap(e, args)) +:
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
          _ <- kw("ref")
          e <- FatorFun
        } yield Ref(e)) +:
        (for {
          _ <- kw("throw")
          (msg, _) <- str
        } yield Throw(msg)) +:
        (for {
          _ <- op("!")
          e <- FatorFun
        } yield Deref(e)) +:
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
          _ <- kw("try")
          etry <- ExpFun
          _ <- kw("catch")
          ecatch <- ExpFun
          _ <- kw("end")
        } yield TryCatch(etry, ecatch)) +:
        (for {
          _ <- kw("try")
          etry <- ExpFun
          _ <- kw("finally")
          efin <- ExpFun
          _ <- kw("end")
        } yield TryFinally(etry, efin)) +:
        (for {
          _ <- kw("try")
          etry <- ExpFun
          _ <- kw("catch")
          ecatch <- ExpFun
          _ <- kw("finally")
          efin <- ExpFun
          _ <- kw("end")
        } yield TryCatchFinally(etry, ecatch, efin)) +:
        (for { _ <- kw("true") } yield Bool(true)) +:
        (for { _ <- kw("false") } yield Bool(false)) +:
        LetFun +:
        LetRecFun +:
        AnonFun +:
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
        } yield Ap(Var(nome), args)) +:
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
//      } yield es.foldRight(ce)({ case ((n, e), c) => Ap(Fun(List(n), c), List(e)) })

    def LetRecFun: Parser[Exp] = for {
      _ <- kw("letrec")
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
    } yield es.foldRight(ce)({ case ((n, e), c) => Let(n, Rec(n, e), c) })

    def fun: Parser[Prog] = for {
      e <- ProgFun
      _ <- space
      _ <- not(pred(c => true), ())
    } yield e

    fun.parse(s)
  }

  // Ações primitivas
  type Acao[+T] = Mem => (Either[T, Erro], Mem)

  implicit class RichAcao[+T](val a: Acao[T]) extends AnyVal {
    def flatMap[U](f: T => Acao[U]): Acao[U] = bind(a, f)
    def map[U](f: T => U): Acao[U] = bind(a, (v: T) => lift(f(v)))
    def withFilter(p: T => Boolean): Acao[T] = a
  }

  def lift[T](v: T): Acao[T] = mem => (Left(v), mem)

  def erro[T](msg: String): Acao[T] = mem => (Right(Erro(msg)), mem)

  def le(l: End): Acao[Valor] = mem => mem.get(l) match {
    case Some(v) => (Left(v), mem)
    case None => (Right(Erro("referência " + l + " não existe")), mem)
  }

  def escreve(l: End, v: Valor): Acao[Valor] = mem => (Left(v), mem + (l -> v))

  def bind[T, U](a: Acao[T], f: T => Acao[U]): Acao[U] =
    mem => {
      a(mem) match {
        case (Right(Erro(msg)), nmem) => (Right(Erro(msg)), nmem)
        case (Left(v), nmem) => {
          val b: Acao[U] = f(v)
          b(nmem)
        }
      }
    }

  def trycatch[T](atry: Acao[T], acatch: Acao[T]): Acao[T] =
    mem => {
      atry(mem) match {
        case (Left(v), nmem) => (Left(v), nmem)
        case (Right(Erro(msg)), nmem) => acatch(nmem)
      }
    }

  def tryfinally[T](atry: Acao[T], afin: Acao[_]): Acao[T] =
    mem => {
      atry(mem) match {
        case (Left(v), mem1) => {
          afin(mem1) match {
            case (Left(_), mem2) => (Left(v), mem2)
            case (Right(Erro(msgf)), mem2) => (Right(Erro(msgf)), mem2)
          }
        }
        case (Right(Erro(msgt)), mem1) => {
          afin(mem1) match {
            case (Left(_), mem2) => (Right(Erro(msgt)), mem2)
            case (Right(Erro(msgf)), mem2) => (Right(Erro(msgf)), mem2)
          }
        }
      }
    }

  def trycatchfinally[T](atry: Acao[T], acatch: Acao[T], afin: Acao[_]): Acao[T] =
    mem => {
      atry(mem) match {
        case (Left(v1), mem1) =>
          afin(mem1) match {
            case (Left(_), mem2) => (Left(v1), mem2)
            case (Right(Erro(msgf)), mem2) => (Right(Erro(msgf)), mem2)
          }
        case (Right(Erro(msgt)), mem1) => {
          acatch(mem1) match {
            case (Left(v2), mem2) =>
              afin(mem2) match {
                case (Left(_), mem3) => (Left(v2), mem3)
                case (Right(Erro(msgf)), mem3) => (Right(Erro(msgf)), mem3)
              }
            case (Right(Erro(msgc)), mem2) =>
              afin(mem2) match {
                case (Left(_), mem3) => (Right(Erro(msgc)), mem3)
                case (Right(Erro(msgf)), mem3) => (Right(Erro(msgf)), mem3)
              }
          }
        }
      }
    }

  def aloca(v: Valor): Acao[Valor] = for {
    Num(l) <- le(0)
    nl <- lift(l.toInt+1)
    _ <- escreve(0, Num(nl.toString))
    _ <- escreve(nl, v)
  } yield Caixa(nl)

  def foldAcao[T](l: List[Acao[T]]): Acao[List[T]] = l match {
    case List() => lift(List())
    case a :: as => for {
      v <- a
      vs <- foldAcao(as)
    } yield v :: vs
  }

  def run(prog: Prog) = {
    val Prog(defs, corpo) = prog
    val tenv = defs.mapValues({ case Proto(ps, c) => Fun(Map(), ps, c) })

    def evnum(env: Env, e: Exp): Acao[BigDecimal] = for {
      v <- eval(env, e)
      ve <- v match {
        case Num(n) => lift(BigDecimal(n))
        case _ => erro("valor " + v + " não é número")
      }
    } yield ve

    def evfun(env: Env, e: Exp): Acao[Fun] = for {
      v <- eval(env, e)
      ve <- v match {
        case f: Fun => lift(f)
        case _ => erro("valor " + v + " não é função")
      }
    } yield ve

    def evcaixa(env: Env, e: Exp): Acao[Caixa] = for {
      v <- eval(env, e)
      ve <- v match {
        case c: Caixa => lift(c)
        case _ => erro("valor " + v + " não é referência")
      }
    } yield ve

    def eval(env: Env, e: Exp): Acao[Valor] = e match {
      case Num(v) => lift(Num(v))
      case Bool(v) => lift(Bool(v))
      case Proto(p, c) => lift(Fun(env, p, c))
      case Soma(e1, e2) => for {
        n1 <- evnum(env, e1)
        n2 <- evnum(env, e2)
      } yield Num((n1 + n2).toString())
      case Mult(e1, e2) => for {
        n1 <- evnum(env, e1)
        n2 <- evnum(env, e2)
      } yield Num((n1 * n2).toString())
      case Div(e1, e2) => for {
        n1 <- evnum(env, e1)
        n2 <- evnum(env, e2)
      } yield Num((n1 / n2).toString())
      case Menor(e1, e2) => for {
        n1 <- evnum(env, e1)
        n2 <- evnum(env, e2)
      } yield Bool(n1 < n2)
      case If(ec, et, ee) => for {
        vc <- eval(env, ec)
        v <- vc match {
          case Bool(c) => if (c) eval(env, et) else eval(env, ee)
          case _ => erro("condição do if não é booleana mas " + vc)
        }
      } yield v
      case Var(nome) => env.get(nome) match {
        case Some(Thunk(env, exp)) => eval(env, exp)
        case Some(ThunkR(env, nome, exp)) =>
          eval(env + (nome -> ThunkR(env, nome, exp)), exp)
        case Some(v) => lift(v)
        case None => erro("variável " + nome + " não existe")
      }
      case Ap(fun, args) => for {
        Fun(fenv, params, corpo) <- evfun(env, fun)
        pargs <- if (params.length == args.length)
          lift(params.zip(args)) else
          erro("aridade incompatível na chamada de função")
        vargs <- foldAcao(pargs.map({ case (p, a) =>
          if(p.startsWith("_")) lift(Thunk(env, a)) else eval(env, a) }))
        vcorpo <- eval(tenv ++ fenv ++ params.zip(vargs).toMap, corpo)
      } yield vcorpo
      case Let(nome, exp, corpo) if nome.startsWith("_") =>
        eval(env + (nome -> Thunk(env, exp)), corpo)
      case Let(nome, exp, corpo) => for {
        ve <- eval(env, exp)
        vl <- eval(env + (nome -> ve), corpo)
      } yield vl
      case Rec(nome, exp) => lift(ThunkR(env, nome, exp))
      case Ref(exp) => for {
        ve <- eval(env, exp)
        vr <- aloca(ve)
      } yield vr
      case Deref(exp) => for {
        Caixa(l) <- evcaixa(env, exp)
        vd <- le(l)
      } yield vd
      case Caixa(l) => lift(Caixa(l))
      case Seq(e1, e2) => for {
        _ <- eval(env, e1)
        v <- eval(env, e2)
      } yield v
      case Atrib(e1, e2) => for {
        v <- eval(env, e2)
        Caixa(l) <- evcaixa(env, e1)
        _ <- escreve(l, v)
      } yield v
      case Throw(msg) => erro(msg)
      case TryCatch(etry, ecatch) => trycatch(eval(env, etry), eval(env, ecatch))
      case TryFinally(etry, efin) => tryfinally(eval(env, etry), eval(env, efin))
      case TryCatchFinally(etry, ecatch, efin) =>
        trycatchfinally(eval(env, etry), eval(env, ecatch), eval(env, efin))
    }

    eval(tenv, corpo)(Map(0 -> Num("0")))
  }

}