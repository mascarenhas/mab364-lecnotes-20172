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

  case class TryCatch(etry: Exp, ecatch: Exp) extends Exp

  case class While(econd: Exp, ecorpo: Exp) extends Exp

  case class Read() extends Exp
  case class Print(args: List[Exp]) extends Exp

  case class Erro(msg: String)

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
        (for { _ <- kw("read") } yield Read()) +:
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
            _ <- kw("try")
            etry <- ExpFun
            _ <- kw("catch")
            ecatch <- ExpFun
            _ <- kw("end")
          } yield TryCatch(etry, ecatch)) +:
          (for {
            _ <- kw("while")
            econd <- ExpFun
            _ <- kw("do")
            ecorpo <- ExpFun
            _ <- kw("end")
          } yield While(econd, ecorpo)) +:
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
          (nome, _) <- kw("print")
          _ <- op("(")
          args <- (for {
            a <- ExpFun
            as <- many(for {
              _ <- op(",")
              a <- ExpFun
            } yield a)
          } yield a :: as) +: empty(List())
          _ <- op(")")
        } yield Print(args)) +:
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

  trait Handlers {
    def tail = {
      val Handler(_, _, n) = this
      n
    }
  }
  case class Handler(val k: Cont[Unit], val sp: End, val next: Handlers) extends Handlers
  case class EmptyH() extends Handlers

  type Resp = (Handlers, Stream[Valor], End, Mem) =>
    (Either[Valor,Erro], Stream[Valor], List[String], End, Mem)

  type Cont[-T] = T => Resp

  // Ações primitivas
  type Acao[+T] = Cont[T] => Resp

  implicit class RichAcao[+T](val a: Acao[T]) extends AnyVal {
    def flatMap[U](f: T => Acao[U]): Acao[U] = bind(a, f)
    def map[U](f: T => U): Acao[U] = bind(a, (v: T) => lift(f(v)))
    def withFilter(p: T => Boolean): Acao[T] = a
  }

  def lift[T](v: T): Acao[T] = k => k(v)

  /*
  // Erro com continuação delimitada no try/catch
  def erro[T](msg: String): Acao[T] = k =>
    (in, sp, mem) => (Right(Erro(msg)), in, List(), sp, mem)
  */

  // Try catch com pilha de tratadores
  def erro[T](msg: String): Acao[T] = k =>
    (handlers, in, sp, mem) => handlers match {
      case Handler(hk, hsp, hrest) => hk(())(hrest, in, hsp, mem)
      case EmptyH() => (Right(Erro(msg)), in, List(), sp, mem)
    }

  val kid: Cont[Valor] = v => (handlers, in, sp, mem) => (Left(v), in, List(), sp, mem)

  /*
  // Try/Catch com continuação delimitada
  def trycatch(atry: Acao[Valor], acatch: Acao[Valor]): Acao[Valor] =
    k => (in, sp, mem) => {
      val (v, nin, nout, nsp, nmem) = atry(kid)(in, sp, mem)
      v match {
        case Left(vv) => {
          val (nv, nnin, nnout, nnsp, nnmem) = k(vv)(nin, nsp, nmem)
          (nv, nnin, nout ++ nnout, nnsp, nnmem)
        }
        case Right(Erro(_)) => {
          val (nv, nnin, nnout, nnsp, nnmem) = acatch(k)(nin, sp, nmem)
          (nv, nnin, nout ++ nnout, nnsp, nnmem)
        }
      }
    }
  */

  // Try/Catch com pilha de tratadores
  def trycatch(atry: Acao[Valor], acatch: Acao[Valor]): Acao[Valor] =
    k => (handlers, in, sp, mem) => {
      atry(v => (handlers, in, sp, mem) => k(v)(handlers.tail, in, sp, mem))(Handler(_ => acatch(k), sp, handlers), in, sp, mem)
    }

  def le(l: End): Acao[Valor] = k => (handlers, in, sp, mem) => mem.get(l) match {
    case Some(v) => k(v)(handlers, in, sp, mem)
    case None => erro("endereço " + l + " não existe")(k)(handlers, in, sp, mem)
  }

  def escreve(l: End, v: Valor): Acao[Valor] =
    k => (handlers, in, sp, mem) => k(v)(handlers, in, sp, mem + (l -> v))

  def bind[T, U](a: Acao[T], f: T => Acao[U]): Acao[U] =
    k => a(v => {
      val b: Acao[U] = f(v)
      b(k)
    })

  def push(v: Valor): Acao[End] = k => (handlers, in, sp, mem) =>
    k(sp)(handlers, in, sp + 1, mem + (sp -> v))

  def pop(n: Int): Acao[Unit] = k => (handlers, in, sp, mem) => k(())(handlers, in, sp - n, mem)

  def print(vs: List[Valor]): Acao[Unit] = k => (handlers, in, sp, mem) => {
    val (v, nin, nout, nsp, nmem) = k(())(handlers, in, sp, mem)
    (v, nin, vs.map(v => v.toString()).mkString("\t") :: nout, nsp, nmem)
  }

  val read: Acao[Valor] = k => (handlers, in, sp, mem) => in.headOption match {
    case Some(v) => k(v)(handlers, in.tail, sp, mem)
    case None => erro("entrada está vazia")(k)(handlers, in, sp, mem)
  }

  def foldAcao[T](l: List[Acao[T]]): Acao[List[T]] = l match {
    case List() => lift(List())
    case a :: as => for {
      v <- a
      vs <- foldAcao(as)
    } yield v :: vs
  }

  def run(prog: Prog, in: Stream[Valor]) = {
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
      case Var(nome) => env.get(nome) match {
        case Some(l) => le(l)
        case None => erro("variável " + nome + " não existe")
      }
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
        val pas = ps.zip(as)
        for {
          vas: List[Valor] <- foldAcao(pas.map({ case (p: String, a: Exp) =>
            if(p.startsWith("_"))
              a match {
                case Var(n) => lift(env(n))
                case Deref(e) => eval(env, e)
                case e => erro("argumento " + e + " não é um lvalue")
              }
            else
              eval(env, a) }))
          ls: List[End] <- foldAcao(ps.zip(vas).map({ case (p: String, va: Valor) =>
            if(p.startsWith("_"))
              lift(va)
            else
              push(va) }))
          v <- eval(ps.zip(ls).toMap, c)
          _ <- pop(ps.count(p => !p.startsWith("_")))
        } yield v
      }
      case TryCatch(etry, ecatch) => trycatch(eval(env, etry), eval(env, ecatch))
      case While(econd, ecorpo) => {
        val acond = eval(env, econd)
        val acorpo = eval(env, ecorpo)
        def loop(): Acao[Valor] = for {
          vc <- acond
          vw <- if (vc != 0) for {
            vw <- acorpo
            vl <- loop()
          } yield vl else lift(0)
        } yield vw
        loop()
      }
      case Print(args) => for {
        vargs <- foldAcao(args.map(a => eval(env, a)))
        _ <- print(vargs)
      } yield 0
      case Read() => read
    }

    eval(Map(), corpo)(kid)(EmptyH(), in, 1000, Map(0 -> 1))
  }
}