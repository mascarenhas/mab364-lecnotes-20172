import parser._

package object proto {
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

  case class Coro(nome: String, args: List[Exp]) extends Exp
  case class Yield(exp: Exp) extends Exp
  case class Resume(exp: Exp) extends Exp

  case class Erro(msg: String)

  case class Object(args: List[Exp], metodos: Map[String, Fun], sup: Option[Exp]) extends Exp
  case class Campo(off: Int) extends Exp
  case class Mensagem(rcv: Exp, nome: String, args: List[Exp]) extends Exp
  case class AtribCampo(off: Int, rval: Exp) extends Exp

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
      _ <- op("@")
      (n, _) <- num
      _ <- op(":=")
      e <- RelFun
    } yield AtribCampo(n.toInt, e)) +: (for {
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

    def FatorFun: Parser[Exp] =
        (for {
          rcv <- ((for {
            _ <- op("@")
            (off, _) <- num
          } yield Campo(off.toInt)) +:
            (for {
            (nome, _ ) <- id
          } yield Var(nome)) +: (for {
            _ <- op("(")
            e <- ExpFun
            _ <- op(")")
          } yield e))
          _ <- op(".")
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
        } yield Mensagem(rcv, nome, args)) +:
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
            _ <- kw("yield")
            _ <- op("(")
            e <- ExpFun
            _ <- op(")")
          } yield Yield(e)) +:
          (for {
            _ <- kw("resume")
            _ <- op("(")
            e <- ExpFun
            _ <- op(")")
          } yield Resume(e)) +:
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
            _ <- kw("object")
            _ <- op("(")
            args <- (for {
              a <- ExpFun
              as <- many(for {
                _ <- op(",")
                a <- ExpFun
              } yield a)
            } yield a :: as) +: empty(List())
            _ <- op(")")
            sup <- opt(for {
              _ <- kw("extends")
              e <- ExpFun
            } yield e)
            funs <- many(DefFun)
            _ <- kw("end")
          } yield Object(args, funs.toMap, sup)) +:
          (for {
            _ <- op("@")
            (off, _) <- num
          } yield Campo(off.toInt)) +:
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
            _ <- kw("coro")
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
          } yield Coro(nome, args)) +:
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
      val Handler(_, _, _, n) = this
      n
    }
  }
  case class Handler(val k: Cont[Unit], val sp: End, val coros: Frames, val next: Handlers) extends Handlers
  case class EmptyH() extends Handlers

  trait Frames  {
    def tail = {
      val Frame(_, _, _, n) = this
      n
    }
  }
  case class Frame(val k: Cont[Valor], val sp: End, val chandle: Valor,
                   val next: Frames) extends Frames
  case class EmptyF() extends Frames

  case class Coroutine(val k: Cont[Valor], val sp: End)
  type Coroutines = (Vector[Coroutine], Frames)
  type Objects = (Vector[ObjectV], End)

  case class ObjectV(campos: End, ncampos: Int, funs: Map[String, Fun], proto: Option[ObjectV])

  type Resp = (Objects, Coroutines, Handlers, Stream[Valor], End, Mem) =>
    (Either[Valor,Erro], List[String])

  type Cont[-T] = T => Resp

  // Ações primitivas
  type Acao[+T] = Cont[T] => Resp

  implicit class RichAcao[+T](val a: Acao[T]) extends AnyVal {
    def flatMap[U](f: T => Acao[U]): Acao[U] = bind(a, f)
    def map[U](f: T => U): Acao[U] = bind(a, (v: T) => lift(f(v)))
    def withFilter(p: T => Boolean): Acao[T] = a
  }

  def lift[T](v: T): Acao[T] = k => k(v)

  // Try catch com pilha de tratadores
  def erro[T](msg: String): Acao[T] = k =>
    (objs, coros, handlers, in, sp, mem) => handlers match {
      case Handler(hk, hsp, hstk, hrest) => {
        val (vec, stk) = coros
        hk(())(objs, (vec, hstk), hrest, in, hsp, mem)
      }
      case EmptyH() => (Right(Erro(msg)), List())
    }

  val kid: Cont[Valor] = v => (objs, coros, handlers, in, sp, mem) => (Left(v), List())

  // Try/Catch com pilha de tratadores
  def trycatch(atry: Acao[Valor], acatch: Acao[Valor]): Acao[Valor] =
    k => (objs, coros, handlers, in, sp, mem) => {
      atry(v => (objs, coros, handlers, in, sp, mem) =>
        k(v)(objs, coros, handlers.tail, in, sp, mem))(objs, coros, Handler(_ => acatch(k), sp, coros._2, handlers), in, sp, mem)
    }

  def le(l: End): Acao[Valor] = k => (objs, coros, handlers, in, sp, mem) => mem.get(l) match {
    case Some(v) => k(v)(objs, coros, handlers, in, sp, mem)
    case None => erro("endereço " + l + " não existe")(k)(objs, coros, handlers, in, sp, mem)
  }

  def escreve(l: End, v: Valor): Acao[Valor] =
    k => (objs, coros, handlers, in, sp, mem) => k(v)(objs, coros, handlers, in, sp, mem + (l -> v))

  def bind[T, U](a: Acao[T], f: T => Acao[U]): Acao[U] =
    k => a(v => {
      val b: Acao[U] = f(v)
      b(k)
    })

  def push(v: Valor): Acao[End] = k => (objs, coros, handlers, in, sp, mem) =>
    k(sp)(objs, coros, handlers, in, sp + 1, mem + (sp -> v))

  def pop(n: Int): Acao[Unit] = k => (objs, coros, handlers, in, sp, mem) => k(())(objs, coros, handlers, in, sp - n, mem)

  def print(vs: List[Valor]): Acao[Unit] = k => (objs, coros, handlers, in, sp, mem) => {
    val (v, nout) = k(())(objs, coros, handlers, in, sp, mem)
    (v, vs.map(v => v.toString()).mkString("\t") :: nout)
  }

  def newobj(vc: List[Valor], funs: Map[String, Fun], vproto: Option[Valor]): Acao[Valor] = k =>
    (objs, coros, handlers, in, sp, mem) => {
      val (vobj, free) = objs
      val (vcampos, proto) = vproto match {
        case Some(hproto) => {
          val oproto = vobj(hproto)
          val ObjectV(pcampos, ncampos, _, _) = oproto
          (Range(pcampos, pcampos+ncampos).map(l => mem(l)) ++ vc, Some(oproto))
        }
        case None => (vc, None)
      }
      k(vobj.length)((vobj :+ ObjectV(free, vcampos.length, funs, proto),
        free + vcampos.length),
        coros, handlers, in, sp, mem ++
          (vcampos.foldLeft((free, Map[End, Valor]()))((p, v) => {
            val (free, m) = p
            (free + 1, m + (free -> v))
          }))._2)
    }

  def getobj(handle: Valor): Acao[ObjectV] = k =>
    (objs, coros, handlers, in, sp, mem) =>
      k(objs._1(handle))(objs, coros, handlers, in, sp, mem)

  val read: Acao[Valor] = k => (objs, coros, handlers, in, sp, mem) => in.headOption match {
    case Some(v) => k(v)(objs, coros, handlers, in.tail, sp, mem)
    case None => erro("entrada está vazia")(k)(objs, coros, handlers, in, sp, mem)
  }

  def foldAcao[T](l: List[Acao[T]]): Acao[List[T]] = l match {
    case List() => lift(List())
    case a :: as => for {
      v <- a
      vs <- foldAcao(as)
    } yield v :: vs
  }

  def optAcao[T](opt: Option[Acao[T]]): Acao[Option[T]] = opt match {
    case Some(a) => for {
      va <- a
    } yield Some(va)
    case None => lift(None)
  }

  def newcoro(corpo: Acao[Valor]): Acao[Valor] = k =>
    (objs, coros, handlers, in, sp, mem) => {
      val (vec, stk) = coros
      val coro = Coroutine(_ => corpo(v => erro("corotina encerrou")(kid)), vec.length * 10000)
      k(vec.length)(objs, (vec :+ coro, stk), handlers, in, sp, mem)
    }

  def search(obj: ObjectV, nome: String): Acao[Fun] = obj.funs.get(nome) match {
    case Some(f) => lift(f)
    case None => obj.proto match {
      case Some(proto) => search(proto, nome)
      case None => erro("método " + nome + " não encontrado")
    }
  }

  def resume(c: Valor): Acao[Valor] = k =>
    (objs, coros, handlers, in, sp, mem) => {
      val (vec, stk) = coros
      val Coroutine(kc, spc) = vec(c)
      kc(0)(objs, (vec, Frame(k, sp, c, stk)), handlers, in, spc, mem)
    }

  def yieldc(v: Valor): Acao[Valor] = kc =>
    (objs, coros, handlers, in, spc, mem) => coros match {
      case (vec, Frame(k, sp, c, next)) => k(v)(objs, (vec.updated(c, Coroutine(kc, spc)), next), handlers, in, sp, mem)
      case (vec, EmptyF()) => erro("yield fora de corotina")(kc)(objs, coros, handlers, in, spc, mem)
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
      case Coro(nome, args) => for {
        vargs <- foldAcao(args.map(a => eval(env, a)))
        c <- newcoro(eval(env, Ap(nome, args)))
      } yield c
      case Resume(e) => for {
        c <- eval(env, e)
        v <- resume(c)
      } yield v
      case Yield(e) => for {
        v <- eval(env, e)
        _ <- yieldc(v)
      } yield 0
      case Object(args, funs, proto) => for {
        vargs <- foldAcao(args.map(a => eval(env, a)))
        vproto <- optAcao(proto.flatMap(e => Some(eval(env, e))))
        handle <- newobj(vargs, funs, vproto)
      } yield handle
      case Campo(off) => for {
        vrcv <- le(env("self"))
        ObjectV(campos, _, funs, _) <- getobj(vrcv)
        vc <- le(campos + off)
      } yield vc
      case AtribCampo(off, rval) => for {
        vrval <- eval(env, rval)
        vrcv <- le(env("self"))
        ObjectV(campos, _, funs, _) <- getobj(vrcv)
        _ <- escreve(campos + off, vrval)
      } yield vrval
      case Mensagem(Var("super"), nome, args) => for {
        vrcv <- le(env("self"))
        vargs <- foldAcao(args.map(a => eval(env, a)))
        lself <- push(vrcv)
        largs <- foldAcao(vargs.map(va => push(va)))
        obj <- getobj(vrcv)
        Fun(params, corpo) <- obj.proto match {
          case Some(proto) => search(proto, nome)
          case None => erro("super não pode ser usado em objeto sem protótipo")
        }
        vc <- eval(params.zip(largs).toMap + ("self" -> lself), corpo)
        _ <- pop(args.length)
      } yield vc
      case Mensagem(rcv, nome, args) => for {
        vrcv <- eval(env, rcv)
        vargs <- foldAcao(args.map(a => eval(env, a)))
        lself <- push(vrcv)
        largs <- foldAcao(vargs.map(va => push(va)))
        obj <- getobj(vrcv)
        Fun(params, corpo) <- search(obj, nome)
        vc <- eval(params.zip(largs).toMap + ("self" -> lself), corpo)
        _ <- pop(args.length)
      } yield vc
    }

    eval(Map(), corpo)(kid)((Vector(), 10000), (Vector(), EmptyF()), EmptyH(), in, 1000, Map(0 -> 1))
  }
}