trait Lista[T] {
  def ::(hd: T): Lista[T] = Cons(hd, this)
  def ++(l: Lista[T]): Lista[T] = this match {
    case Vazia() => l
    case Cons(hd, tl) => hd :: (tl ++ l)
  }
  def +(hd: T): Lista[T] = Cons(hd, this)
}
case class Vazia[T]() extends Lista[T]
case class Cons[T](hd: T, tl: Lista[T]) extends Lista[T]

def tamanho[T](l: Lista[T]): Int = l match {
  case Vazia() => 0
  case Cons(hd, tl) => 1 + tamanho(tl)
}

def terceiro[T](l: Lista[T]): T = {
  val Cons(_, Cons(_, Cons(e, _))) = l
  e
}

val l3 = Vazia() + 1 + 2 + 3

val l: Lista[Int] = 1 :: (2 :: (3 :: Vazia()))
val l2: Lista[Int] = Vazia().::(3).::(2).::(1)

l ++ l2

terceiro(l)
//terceiro(Vazia())

l == Cons(1,Cons(2,Cons(3,Vazia())))

def terceiroOpt[T](l: Lista[T]): Option[T] =l match {
  case Cons(_, Cons(_, Cons(e, _))) => Some(e)
  case _ => None
}

for {
  x <- terceiroOpt(l)
  y <- Some(2)//terceiroOpt(Vazia[Int]())
} yield x + y

terceiroOpt(l).flatMap(x => for {
  y <- Some(2)//terceiroOpt(Vazia[Int]())
} yield x + y)

trait Opcao[T] {
  def flatMap[U](f: T => Opcao[U]): Opcao[U] = this match {
    case Nada() => Nada()
    case Algum(x) => f(x)
  }
  def map[U](f: T => U): Opcao[U] = this match {
    case Nada() => Nada()
    case Algum(x) => Algum(f(x))
  }
}
case class Nada[T]() extends Opcao[T]
case class Algum[T](x: T) extends Opcao[T]

terceiroOpt(l).flatMap(x =>
  Some(2).map(y => x + y))

for {
  x <- Algum(3)
  y <- Algum(2)
} yield x + y

trait ArvoreBin[T] {
  def fold[U](f: T => U, r: (T, U, U) => U):
        U =
    this match {
      case Folha(x) => f(x)
      case Ramo(x, e, d) =>
        r(x, e.fold(f, r), d.fold(f, r))
    }
}
case class Folha[T](rot: T) extends ArvoreBin[T]
case class Ramo[T](rot: T,
                   esq: ArvoreBin[T],
                   dir: ArvoreBin[T]) extends ArvoreBin[T]

val a: ArvoreBin[Int] = Ramo(2, Folha(3),
  Ramo(4, Folha(1), Folha(5)))

def altura[T](a: ArvoreBin[T]): Int = a match {
  case Folha(_) => 1
  case Ramo(_, esq, dir) =>
    1 + Math.max(altura(esq), altura(dir))
}

altura(a)

def soma3(x: Int, y: Int, z: Int) = x + y + z
def id[T](x: T) = x

def inv3[T](x: T, e: ArvoreBin[T], d: ArvoreBin[T]) = Ramo(x, d, e)

soma3(2,id(3),soma3(4,id(1),id(5)))

inv3(2,Folha(3),inv3(4,Folha(1),Folha(5)))

a.fold(id, soma3)
def inverte1[T](a: ArvoreBin[T]): ArvoreBin[T] = a.fold(
    x => Folha(x),
    (x: T, e: ArvoreBin[T], d: ArvoreBin[T])
        => Ramo(x, d, e)
  )

inverte1(a)

def inverte2[T](a: ArvoreBin[T]):
  ArvoreBin[T] = a match {
    case Folha(x) => Folha(x)
    case Ramo(x, e, d) =>
      Ramo(x, inverte2(d), inverte2(e))
  }

inverte2(a)