package object lista1  {
  // defina as funções da sua resposta aqui

  def pascal(col: Int, lin: Int): Int = ???

  def balanceado(l: List[Char]): Boolean = ???

  def particao(l: List[Int], pivo: Int): (List[Int], List[Int]) = ???
  def quicksort(l: List[Int]): List[Int] = ???

  def crSemestre(notas: List[(Double, Int)]): (Double, Int) = ???
  def crsAcumulados(semestres: List[List[(Double, Int)]]): (List[Double], Int) = ???

  type Conjunto[T] = T => Boolean

  def contem[T](conj: Conjunto[T], elem: T) = conj(elem)
  def unitario[T](elem: T): Conjunto[T] = ???
  def uniao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = ???
  def intersecao[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = ???
  def diferenca[T](c1: Conjunto[T], c2: Conjunto[T]): Conjunto[T] = ???
  def filtro[T](c: Conjunto[T], f: T => Boolean): Conjunto[T] = ???
  def map[T, U](c: Conjunto[T], f: U => T): Conjunto[U] = ???

  trait ConjInt {
    def contem(x: Int): Boolean = ???
    def insere(x: Int): ConjInt = ???
    def uniao(outro: ConjInt): ConjInt = ???

    def filter(p: Int => Boolean): ConjInt = ???
    def map(f: Int => Int): ConjInt = ???
    def flatMap(f: Int => ConjInt): ConjInt = ???
  }
  case class ConjVazio() extends ConjInt
  case class ConjCons(elem: Int, esq: ConjInt, dir: ConjInt) extends ConjInt

  def intersecao(c1: ConjInt, c2: ConjInt): ConjInt = ???

}
