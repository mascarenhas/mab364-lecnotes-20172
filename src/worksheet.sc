val l = 1 :: 2 :: 3 :: Nil
Nil == List()
l.head
l.tail
l.isEmpty

def concat[T](l1: List[T], l2: List[T]): List[T] =
  if (l1.isEmpty) l2
  else l1.head :: concat(l1.tail, l2)

concat(List(1, 2, 3), List(4, 5))

List(1, 2, 3) ++ List(4, 5)

def elem[T](i: Int, l: List[T]): T =
  if (i == 0) l.head
  else elem(i - 1, l.tail)

def concatP[T](l1: List[T], l2: List[T]): List[T] =
  l1 match {
    case List() => l2
    case h :: t => h :: concatP(t, l2)
  }

def tamanho[T](l: List[T]): Int = l match {
  case Nil => 0
  case _ :: t => 1 + tamanho(t)
}

def append[T](l: List[T], e: T): List[T] = l match {
  case List() => List(e)
  case h :: t => h :: append(t, e)
}

def map[T,U](f: T => U)(l: List[T]): List[U] = l match {
  case Nil => Nil
  case h :: t => f(h) :: map(f)(t)
}

def mult(l: List[Double], fator: Double): List[Double] =
    map((x: Double) => x * fator)(l)

mult(List(1,2,3,4), 3)

def filter[T](pred: T => Boolean)(l: List[T]): List[T] = l match {
  case Nil => Nil
  case h :: t => if (pred(h)) h :: filter(pred)(t)
  else filter(pred)(t)
}

def filtraMaiores(l: List[Double], pivo: Double): List[Double] =
  filter((x: Double) => x > pivo)(l)

def particao(l: List[Double], pivo: Double) =
  (filter((x: Double) => x <= pivo)(l),
   filter((x: Double) => x > pivo)(l))

particao(List(4, 1, 8, 5, 3), 4)

def reduceRight[T](l: List[T], op: (T, T) => T): T = l match {
  case Nil => sys.error("lista vazia")
  case h :: Nil => h
  case h :: t => op(h, reduceRight(t, op))
}

def soma(l: List[Int]): Int = reduceRight(l,
    (x: Int, y: Int) => x + y)

soma(List(1,3,5,7,9))

def reduceLeft[T](l: List[T], op: (T, T) => T): T = {
  def loop(l: List[T], acum: T): T = l match {
    case Nil => acum
    case h :: t => loop(t, op(acum, h))
  }
  l match {
    case Nil => sys.error("lista vazia")
    case h :: t => loop(t, h)
  }
}

def somaRF(l: List[Int]): Int = reduceLeft(l,
  (x: Int, y: Int) => x + y)

somaRF(List(1,3,5,7,9))

def foldRight[T,U](l: List[T], z: U, op: (T, U) => U): U = l match {
  case Nil => z
  case h :: t => op(h, foldRight(t, z, op))
}

def foldLeft[T,U](l: List[T], z: U, op: (U, T) => U): U = l match {
  case Nil => z
  case h :: t => foldLeft(t, op(z, h), op)
}

def concatFR[T](l1: List[T], l2: List[T]) =
  foldRight(l1, l2, (e: T, l: List[T]) => e :: l)

concatFR(List(1, 2, 3), List(4, 5))

def concatFL[T](l1: List[T], l2: List[T]) =
  foldLeft(l2, l1, (l: List[T], e: T) => append(l, e))

concatFL(List(1, 2, 3), List(4, 5))

def tamanhoFL[T](l: List[T]) =
  foldLeft(l, 0, (t: Int, e: T) => t + 1)

tamanhoFL(concatFL(List(1, 2, 3), List(4, 5))
)

def appendFR[T](l: List[T], e: T): List[T] =
  foldRight(l, List(e), (e: T, z: List[T]) => e :: z)

def mapFR[T,U](f: T => U)(l: List[T]): List[U] =
  foldRight(l, List(), (e: T, z: List[U]) => f(e) :: z)

List(1,2,3,4,5).foldLeft(0)((z, x) => z + x)

List(1,2,3,4,5).flatMap(x => List(5,6,7).map(y => (x, y)))

for {
  x <- List(1,2,3,4,5)
  if x % 2 == 1
  y <- List(5,6,7)
  if x % y != 0 && y % x != 0
} yield (x, y)

for {
  x <- List(1,2,3,4,5).filter(x => x % 2 == 1)
  y <- List(5,6,7)
  if x % y != 0 && y % x != 0
} yield (x, y)

List(1,2,3,4,5).filter(x => x % 2 == 1).flatMap(x =>
for {
  y <- List(5,6,7)
  if x % y != 0 && y % x != 0
} yield (x, y))

List(1,2,3,4,5).filter(x => x % 2 == 1).flatMap(x =>
  for {
    y <- List(5,6,7).filter(y => x % y != 0 && y % x != 0)
  } yield (x, y))

List(1,2,3,4,5).filter(x => x % 2 == 1).flatMap(x =>
    List(5,6,7).filter(y => x % y != 0 && y % x != 0).map(y => (x, y)))
