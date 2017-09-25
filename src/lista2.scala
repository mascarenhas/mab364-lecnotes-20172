package object lista2 {
  trait CL
  case class Var(nome: String) extends CL
  case class Abs(param: String, corpo: CL) extends CL
  case class Ap(fun: CL, arg: CL) extends CL

  var i = 0

  def fresh: String = {
    i = i + 1
    "$v" + i
  }

  def fvs(e: CL): Set[String] = e match {
    case Var(nome) => Set(nome)
    case Ap(fun, arg) => fvs(fun) ++ fvs(arg)
    case Abs(param, corpo) => fvs(corpo) - param
  }

  def rename(oque: String, peloque: String, onde: CL): CL = onde match {
    case Var(nome) if nome == oque => Var(peloque)
    case Var(nome) => Var(nome)
    case Ap(fun, arg) => Ap(rename(oque, peloque, fun),
      rename(oque, peloque, arg))
    case Abs(param, corpo) if param == oque => Abs(param, corpo)
    case Abs(param, corpo) => Abs(param, rename(oque, peloque, corpo))
  }

  def subst(oque: String, peloque: CL, onde: CL): CL = onde match {
    case Var(nome) if nome == oque =>
      fvs(peloque).foldLeft(peloque)((acc: CL, fv: String) =>
          rename(fv, fresh, acc))
    /*{
      var acc = peloque
      fvs(peloque).foreach(fv => {  // for(String fv: fvs(peloque)) em Java
        acc = rename(fv, fresh, acc)
      })
      acc
    }*/
    case Var(nome) => Var(nome)
    case Ap(fun, arg) => Ap(subst(oque, peloque, fun),
      subst(oque, peloque, arg))
    case Abs(param, corpo) if param == oque => Abs(param, corpo)
    /* // Alpha-conversion
    case Abs(param, corpo) if fvs(peloque).contains(param) => {
      val nv = fresh
      Abs(nv, subst(oque, peloque, rename(param, nv, corpo)))
    } */
    case Abs(param, corpo) => Abs(param, subst(oque, peloque, corpo))
  }

  // semântica "big-step"
  def eval(e: CL): Abs = e match {
    // omitir esse caso também está correto
    case Var(nome) => sys.error(nome + " não existe")
    case Abs(param, corpo) => Abs(param, corpo)
    case Ap(fun, arg) => {
      val Abs(param, corpo) = eval(fun)
      eval(subst(param, eval(arg), corpo))
    }
  }

  // semântica "big-step"
  def eval_cbn(e: CL): Abs = e match {
    // omitir esse caso também está correto
    case Var(nome) => sys.error(nome + " não existe")
    case Abs(param, corpo) => Abs(param, corpo)
    case Ap(fun, arg) => {
      val Abs(param, corpo) = eval_cbn(fun)
      eval_cbn(subst(param, arg, corpo))
    }
  }

  // semântica "small-step"
  def step(e: CL): CL = e match {
    // omitir esse caso também está correto
    case Var(nome) => sys.error(nome + " não existe")
    case Abs(param, corpo) => Abs(param, corpo)
    case Ap(Abs(param, corpo), v: Abs) => subst(param, v, corpo)
    case Ap(f: Abs, arg) => Ap(f, step(arg))
    case Ap(f, arg) => Ap(step(f), arg)
  }

  // semântica "small-step"
  def step_cbn(e: CL): CL = e match {
    // omitir esse caso também está correto
    case Var(nome) => sys.error(nome + " não existe")
    case Abs(param, corpo) => Abs(param, corpo)
    case Ap(Abs(param, corpo), arg) => subst(param, arg, corpo)
    case Ap(f, arg) => Ap(step(f), arg)
  }

  def stepn(e: CL, cbv: Boolean = true): Abs = e match {
    case v: Abs => v
    case _ => stepn(if (cbv) step(e) else step_cbn(e), cbv)
  }
}