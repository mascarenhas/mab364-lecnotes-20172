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

  def aloca(v: Valor, mem: Mem): (Valor, Mem) = {
    val Num(l) = mem(0)
    val nl = l.toInt + 1
    (Caixa(nl), mem + (0 -> Num(nl.toString),
      nl -> v))
  }

  def run(prog: Prog) = {
    val Prog(defs, corpo) = prog

    def eval(e: Exp, m: Mem): (Valor, Mem) = e match {
      case Erro(e) => (Erro(e), m)
      case Num(v) => (Num(v), m)
      case Bool(v) => (Bool(v), m)
      case Fun(p, c) => (Fun(p, c), m)
      case Soma(e1, e2) => eval(e1, m) match {
        case (Num(n1), m1) => eval(e2, m1) match {
          case (Num(n2), m2) => (Num((BigDecimal(n1) +
            BigDecimal(n2)).toString()), m2)
          case (Erro(msg), m2) => (Erro(msg), m1)
          case (_, m2) => (Erro("aritmética precisa de números"), m2)
        }
        case (Erro(msg), m1) => (Erro(msg), m1)
        case (_, m1) => (Erro("aritmética precisa de números"), m1)
      }
      case Mult(e1, e2) => eval(e1, m) match {
        case (Num(n1), m1) => eval(e2, m1) match {
          case (Num(n2), m2) => (Num((BigDecimal(n1) *
            BigDecimal(n2)).toString()), m2)
          case (Erro(msg), m2) => (Erro(msg), m1)
          case (_, m2) => (Erro("aritmética precisa de números"), m2)
        }
        case (Erro(msg), m1) => (Erro(msg), m1)
        case (_, m1) => (Erro("aritmética precisa de números"), m1)
      }
      case Div(e1, e2) => eval(e1, m) match {
        case (Num(n1), m1) => eval(e2, m1) match {
          case (Num(n2), m2) => (Num((BigDecimal(n1) /
            BigDecimal(n2)).toString()), m2)
          case (Erro(msg), m2) => (Erro(msg), m1)
          case (_, m2) => (Erro("aritmética precisa de números"), m2)
        }
        case (Erro(msg), m1) => (Erro(msg), m1)
        case (_, m1) => (Erro("aritmética precisa de números"), m1)
      }
      case Menor(e1, e2) => eval(e1, m) match {
        case (Num(n1), m1) => eval(e2, m1) match {
          case (Num(n2), m2) => (Bool((BigDecimal(n1) <
            BigDecimal(n2))), m2)
          case (Erro(msg), m2) => (Erro(msg), m1)
          case (_, m2) => (Erro("comparação precisa de números"), m2)
        }
        case (Erro(msg), m1) => (Erro(msg), m1)
        case (_, m1) => (Erro("comparação precisa de números"), m1)
      }
      case If(ec, et, ee) => eval(ec, m) match {
        case (Bool(v), nm) => if (v) eval(et, nm) else eval(ee, nm)
        case (Erro(msg), nm) => (Erro(msg), nm)
        case (_, nm) => (Erro("if precisa de um booleano"), nm)
      }
      case Var(nome) => defs.get(nome) match {
        case Some(f) => (f, m)
        case None => (Erro("variável " + nome + " não existe"), m)
      }
      case FVar(nome) => defs.get(nome) match {
        case Some(f) => (f, m)
        case None => (Erro("variável " + nome + " não existe"), m)
      }
      case Ap(fun, args) => eval(fun, m) match {
        case (Erro(msg), mf) => (Erro(msg), mf)
        case (Fun(params, corpo), mf) => {
          if(params.size == args.size) {
            val pargs: List[(String, Exp)] =
              params.zip(args)
            val pargs2: (List[(String, Exp)], Mem) =
              pargs.foldRight((List[(String, Exp)](), mf))(
                (a: (String, Exp), s: (List[(String, Exp)], Mem)) => {
                  val (vs, m) = s
                  val (n, arg) = a
                  if (n.startsWith("_")) ((n, arg) :: vs, m) else
                   {
                      val (v, nm) = eval(arg, m)
                     ((n, v) :: vs, nm)
                   }
                })
            val smap: Map[String, Exp] = pargs2._1.toMap
            val scorpo = subst(smap, corpo)
            eval(scorpo, pargs2._2)
          } else (Erro("aridade incompatível chamando " + fun), mf)
        }
        case (e, mf) => (Erro(e + " não é uma função"), mf)
      }
      case Let(nome, exp, corpo) if nome.startsWith("_") =>
        eval(subst(Map(nome -> exp), corpo), m)
      case Let(nome, exp, corpo) => eval(exp, m) match {
        case (Erro(msg), nm) => (Erro(msg), nm)
        case (v, nm) => eval(subst(Map(nome -> v), corpo), nm)
      }
      case Rec(nome, exp) =>
        eval(subst(Map(nome -> Rec(nome, exp)), exp), m)
      case Ref(exp) => eval(exp, m) match {
        case (Erro(msg), nm) => (Erro(msg), nm)
        case (v, nm) => aloca(v, nm)
      }
      case Deref(exp) => eval(exp, m) match {
        case (Erro(msg), nm) => (Erro(msg), nm)
        case (Caixa(l), nm) => (nm(l), nm)
        case (e, nm) => (Erro("tentando dereferenciar " + e), nm)
      }
      case Caixa(l) => (Caixa(l), m)
      case Seq(e1, e2) => {
        val (_, nm) = eval(e1, m)
        eval(e2, nm)
      }
      /*case Atrib(e1, e2) => eval(e1, m) match {
        case (Erro(msg), m1) => (Erro(msg), m1)
        case (Caixa(l), m1) => eval(e2, m1) match {
          case (Erro(msg), m2) => (Erro(msg), m2)
          case (v, m2) => (v, m2 + (l -> v))
        }
        case (e, m1) => (Erro("tentando atribuir " + e), m1)
      }*/
      case Atrib(e1, e2) => eval(e2, m) match {
        case (Erro(msg), m1) => (Erro(msg), m1)
        case (v, m1) => eval(e1, m1) match {
          case (Erro(msg), m2) => (Erro(msg), m2)
          case (Caixa(l), m2) => (v, m2 + (l -> v))
          case (e, m2) => (Erro("tentando atribuir " + e), m2)
        }
      }
    }

    eval(subst(Map(), corpo), Map(0 -> Num("0")))
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