import parser._

package object fun {
  type End = Int
  type Mem = Map[End, Valor]

  trait Valor <: Exp

  trait Exp {
    def +(e: Exp) = Soma(this, e)
    def *(e: Exp) = Mult(this, e)
    def -(e: Exp) = Soma(this, Neg(e))
    def <(e: Exp) = Menor(this, e)

    def fvs: Set[String] = this match {
      case Var(x) => Set(x)
      case Soma(e1, e2) => e1.fvs ++ e2.fvs
      case Mult(e1, e2) => e1.fvs ++ e2.fvs
      case Menor(e1, e2) => e1.fvs ++ e2.fvs
      case If(c, et, ee) => c.fvs ++ et.fvs ++ ee.fvs
      case Let(x, e, c) => e.fvs ++ (c.fvs - x)
      case Fun(ps, c) => c.fvs -- ps
      case Ap(f, as) => f.fvs ++
        as.foldRight(Set[String]())((a, s) => a.fvs ++ s)
      case Rec(n, e) => e.fvs - n
      case Ref(e) => e.fvs
      case Deref(e) => e.fvs
      case Seq(e1, e2) => e1.fvs ++ e2.fvs
      case Atrib(e1, e2) => e1.fvs ++ e2.fvs
      case _ => Set()
    }
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

  case class Fun(params: List[String], corpo: Exp) extends Valor
  case class Prog(defs: Map[String, Fun], corpo: Exp)
  case class Var(nome: String) extends Exp
  case class Ap(fun: Exp, args: List[Exp]) extends Exp

  case class Let(nome: String, exp: Exp, corpo: Exp)
    extends Exp
  case class FVar(nome: String) extends Exp

  case class Rec(nome: String, corpo: Exp) extends Exp

  case class Seq(e1: Exp, e2: Exp) extends Exp
  case class Atrib(lval: Exp, rval: Exp) extends Exp
  case class Ref(e: Exp) extends Exp
  case class Deref(l: Exp) extends Exp

  case class Caixa(l: End) extends Valor

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

    // fun '(' [id {, id}] ')' exp end
    def AnonFun: Parser[Fun] = for {
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
    } yield Fun(params, corpo)

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
    case Ap(fun, args) => Ap(
      subst(smap, fun),
      args.map(arg => subst(smap, arg)))
    case Var(nome) => smap.get(nome) match {
      case Some(v) => {
        val fvs = v.fvs
        if(fvs.isEmpty) v else {
          val fvsmap: Map[String, Exp] =
            fvs.map(fv => (fv, FVar(fv))).toMap
          subst(fvsmap, v)
        }
      }
      case None => e
    }
    case Fun(params, corpo) =>
      Fun(params, subst(smap -- params, corpo))
    case Let(nome, en, ec) =>
      Let(nome, subst(smap, en), subst(smap - nome, ec))
    case Rec(nome, exp) => Rec(nome, subst(smap - nome, exp))
    case Seq(e1, e2) => Seq(subst(smap, e1), subst(smap, e2))
    case Ref(e) => Ref(subst(smap, e))
    case Deref(e) => Deref(subst(smap, e))
    case Atrib(e1, e2) => Atrib(subst(smap, e1), subst(smap, e2))
    case _ => e
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

    def evnum(e: Exp): Acao[BigDecimal] = for {
      v <- eval(e)
      ve <- v match {
        case Num(n) => lift(BigDecimal(n))
        case _ => erro("valor " + v + " não é número")
      }
    } yield ve

    def evfun(e: Exp): Acao[Fun] = for {
      v <- eval(e)
      ve <- v match {
        case f: Fun => lift(f)
        case _ => erro("valor " + v + " não é função")
      }
    } yield ve

    def evcaixa(e: Exp): Acao[Caixa] = for {
      v <- eval(e)
      ve <- v match {
        case c: Caixa => lift(c)
        case _ => erro("valor " + v + " não é referência")
      }
    } yield ve

    def eval(e: Exp): Acao[Valor] = e match {
      case Num(v) => lift(Num(v))
      case Bool(v) => lift(Bool(v))
      case Fun(p, c) => lift(Fun(p, c))
      case Soma(e1, e2) => for {
        n1 <- evnum(e1)
        n2 <- evnum(e2)
      } yield Num((n1 + n2).toString())
      case Mult(e1, e2) => for {
        n1 <- evnum(e1)
        n2 <- evnum(e2)
      } yield Num((n1 * n2).toString())
      case Div(e1, e2) => for {
        n1 <- evnum(e1)
        n2 <- evnum(e2)
      } yield Num((n1 / n2).toString())
      case Menor(e1, e2) => for {
        n1 <- evnum(e1)
        n2 <- evnum(e2)
      } yield Bool(n1 < n2)
      case If(ec, et, ee) => for {
        vc <- eval(ec)
        v <- vc match {
          case Bool(c) => if (c) eval(et) else eval(ee)
          case _ => erro("condição do if não é booleana mas " + vc)
        }
      } yield v
      case Var(nome) => defs.get(nome) match {
        case Some(fun) => lift(fun)
        case None => erro("variável " + nome + " não existe")
      }
      case FVar(nome) => defs.get(nome) match {
        case Some(fun) => lift(fun)
        case None => erro("variável " + nome + " não existe")
      }
      case Ap(fun, args) => for {
        Fun(params, corpo) <- evfun(fun)
        pargs <- if (params.length == args.length)
          lift(params.zip(args)) else
          erro("aridade incompatível na chamada de função")
        vargs <- foldAcao(pargs.map({ case (p, a) =>
          if(p.startsWith("_")) lift(a) else eval(a) }))
        vcorpo <- eval(subst(params.zip(vargs).toMap, corpo))
      } yield vcorpo
      case Let(nome, exp, corpo) if nome.startsWith("_") =>
        eval(subst(Map(nome -> exp), corpo))
      case Let(nome, exp, corpo) => for {
        ve <- eval(exp)
        vl <- eval(subst(Map(nome -> ve), corpo))
      } yield vl
      case Rec(nome, exp) =>
        eval(subst(Map(nome -> Rec(nome, exp)), exp))
      case Ref(exp) => for {
        ve <- eval(exp)
        vr <- aloca(ve)
      } yield vr
      case Deref(exp) => for {
        Caixa(l) <- evcaixa(exp)
        vd <- le(l)
      } yield vd
      case Caixa(l) => lift(Caixa(l))
      case Seq(e1, e2) => for {
        _ <- eval(e1)
        v <- eval(e2)
      } yield v
      case Atrib(e1, e2) => for {
        v <- eval(e2)
        Caixa(l) <- evcaixa(e1)
        _ <- escreve(l, v)
      } yield v
    }

    eval(subst(Map(), corpo))(Map(0 -> Num("0")))
  }

  def stepper(prog: Prog): Exp => Exp = {
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

      case Var(nome) => defs.get(nome) match {
        case Some(f) => f
        case None => Erro("variável " + nome + " não existe")
      }
      case FVar(nome) => defs.get(nome) match {
        case Some(f) => f
        case None => Erro("variável " + nome + " não existe")
      }

      case Ap(Fun(params, corpo), args) => {
        if(params.size == args.size) {
          def stepl(a: List[(String, Exp)]):
          (List[Exp], Boolean) =
            a match {
              case List() => (List(), false)
              case (n, a) :: as
                if n.startsWith("_") => {
                val (nas, b) = stepl(as)
                (a :: nas, b)
              }
              case (n, a: Valor) :: as => {
                val (nas, b) = stepl(as)
                (a :: nas, b)
              }
              case (n, a: Exp) :: as =>
                (step(a) :: as.map({ case (_, a) => a }),
                  true)
            }
          val (nargs, stepp) = stepl(params.zip(args))
          if (stepp)
            Ap(Fun(params, corpo), nargs)
          else {
            val smap: Map[String, Exp] =
              params.zip(args).toMap
            subst(smap, corpo)
          }
        } else Erro("aridade incompatível chamando função")
      }
      case Ap(v: Valor, args) => Erro(v + " não é uma função")
      case Ap(f, args) => Ap(step(f), args)

      case Let(_, Erro(s), _) => Erro(s)
      case Let(n, el: Valor, ec) =>
        subst(Map(n -> el), ec)
      case Let(n, el, ec) if n.startsWith("_") =>
        subst(Map(n -> el), ec)
      case Let(n, el, ec) =>
        Let(n, step(el), ec)

      case Rec(nome, exp) => subst(Map(nome -> Rec(nome, exp)), exp)
    }

    step
  }

  def runs(prog: Prog) = {
    val step = stepper(prog)
    val Prog(defs, corpo) = prog

    def stepn(e: Exp): Valor = {
      println(e)
      e match {
        case Num(v) => Num(v)
        case Bool(v) => Bool(v)
        case Erro(v) => Erro(v)
        case _ => stepn(step(e))
      }
    }

    stepn(corpo)
  }
}