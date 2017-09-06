package object lista1  {
  // defina as funções da sua resposta aqui

  def pascal(col: Int, lin: Int): Int =
    if (col == 0 || col == lin)
      1
    else
      pascal(col, lin - 1) + pascal(col - 1, lin - 1)

  def balanceado(l: List[Char]): Boolean = {
    def loop(l: List[Char], nivel: Int): Boolean = l match {
      case '(' :: t => loop(t, nivel+1)
      case ')' :: t => if (nivel == 0)
        false else loop(t, nivel-1)
      case h :: t => loop(t, nivel)
      case List() => nivel == 0
    }
    loop(l, 0)
  }

  def particao(l: List[Int], pivo: Int): (List[Int], List[Int]) = {
    def loop(l: List[Int], r: (List[Int], List[Int]), pivo: Int):
        (List[Int], List[Int]) = (l, r) match {
      case (h :: t, (l1, l2)) => if (h <= pivo)
          loop(t, (h :: l1, l2), pivo)
        else
          loop(t, (l1, h :: l2), pivo)
      case (List(), r) => r
    }
    loop(l, (List(), List()), pivo)
  }
  def quicksort(l: List[Int]): List[Int] = l match {
    case h :: t => {
      val (l1, l2) = particao(t, h)
      quicksort(l1) ++ List(h) ++ quicksort(l2)
    }
    case List() => List()
  }

  def quicksortH(l: List[Int]): List[Int] = l match {
    case h :: t =>
      quicksortH(t.filter(x => x <= h)) ++ List(h) ++
        quicksortH(t.filter(x => x > h))
    case List() => List()
  }

  def crSemestre(notas: List[(Double, Int)]): (Double, Int) =
    notas.reduceLeft((acum, materia) => {
      val (cra, creditosa) = acum
      val (nota, credito) = materia
      ((cra * creditosa + nota * credito) / (creditosa + credito),
        creditosa + credito)
    })

  def crsAcumulados(semestres: List[List[(Double, Int)]]):
    (List[Double], Int) = {
    val acumSemestres: List[(Double, Int)] = semestres.map(crSemestre)
    acumSemestres.foldLeft((List[Double](), 0))((acum, semestre) => {
      val (cras, creditos) = acum
      val (craSem, creditosSem) = semestre
      (cras ++ List((cras.lastOption.getOrElse(0.0) *
        creditos + craSem * creditosSem) /
        (creditos + creditosSem)), creditos + creditosSem)
    })
  }

  type Conjunto[T] = T => Boolean

  def contem[T](conj: Conjunto[T], elem: T) = conj(elem)
  def unitario[T](elem: T): Conjunto[T] = (x: T) => x == elem
  def uniao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] =
    (x: T) => contem(c1, x) || contem(c2, x)
  def intersecao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] =
    (x: T) => contem(c1, x) && contem(c2, x)
  def diferenca[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] =
    (x: T) => contem(c1, x) && !contem(c2, x)
  def filtro[T](c: Conjunto[T], f: T => Boolean): Conjunto[T] =
    intersecao(c, f)
  def map[T, U](c: Conjunto[T], f: U => T): Conjunto[U] =
    (x: U) => contem(c, f(x))

  trait ConjInt {
    def contem(x: Int): Boolean = this match {
      case ConjVazio() => false
      case ConjCons(elem, esq, dir) =>
        if (elem == x) true
        else if (x < elem) esq.contem(x)
        else dir.contem(x)
    }
    def insere(x: Int): ConjInt =
      this match {
        case ConjVazio() => ConjCons(x, ConjVazio(), ConjVazio())
        case ConjCons(elem, esq, dir) =>
          if (x == elem) ConjCons(elem, esq, dir)
          else if (x < elem) ConjCons(elem, esq.insere(x), dir)
          else ConjCons(elem, esq, dir.insere(x))
      }
    def uniao(outro: ConjInt): ConjInt = outro match {
      case ConjVazio() => this
      case ConjCons(elem, esq, dir) => this.insere(elem).uniao(esq).uniao(dir)
    }

    def filter(p: Int => Boolean): ConjInt = this match {
      case ConjVazio() => this
      case ConjCons(elem, esq, dir) =>
        if (p(elem)) ConjCons(elem, esq.filter(p), dir.filter(p))
        else esq.filter(p).uniao(dir.filter(p))
    }

    def withFilter(p: Int => Boolean): ConjInt = this.filter(p)
    def map(f: Int => Int): ConjInt = this match {
      case ConjVazio() => ConjVazio()
      case ConjCons(elem, esq, dir) => esq.map(f).uniao(dir.map(f)).
        insere(f(elem))
    }
    def flatMap(f: Int => ConjInt): ConjInt = this match {
      case ConjVazio() => ConjVazio()
      case ConjCons(elem, esq, dir) => esq.flatMap(f).uniao(dir.flatMap(f)).
        uniao(f(elem))
    }
  }
  case class ConjVazio() extends ConjInt
  case class ConjCons(elem: Int, esq: ConjInt, dir: ConjInt) extends ConjInt

  def intersecao(c1: ConjInt, c2: ConjInt): ConjInt = for {
    x <- c1
    y <- c2
    if x == y
  } yield x

}
