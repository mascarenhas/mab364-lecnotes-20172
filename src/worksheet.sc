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

val tup = (1, "foo", List(1,2,3))
val (t1, t2, t3) = tup
t1
t2
t3
val tup2: (Int) = (4)
val tup3 = 4
tup2 == tup3

val (xtup2) = tup2
val (ytup3) = tup3

type Zipper[T] = (List[T], T, List[T])

def zipper[T](l: List[T]): Zipper[T] = (List(), l.head, l.tail)

val z = zipper(List(1,2,3,4,5))
val t: (List[Int], Int, List[Int]) = z

def paraFrente[T](z: Zipper[T]): Zipper[T] = z match {
  case (e, f, List()) => sys.error("no final do zipper")
  case (e, f, h :: t) => (f :: e, h, t)
}

def paraTras[T](z: Zipper[T]): Zipper[T] = z match {
  case (List(), f, d) => sys.error("no início do zipper")
  case (h :: t, f, d) => (t, h, f :: d)
}

def append[T](l: List[T], e: T): List[T] = l match {
  case List() => List(e)
  case h :: t => h :: append(t, e)
}

val z1 = paraFrente(paraFrente(z))
val z2 = paraTras(paraTras(z1))
z == z2

def soma(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + soma(f, a + 1, b)

def id(x: Int) = x
def quad(x: Int) = x * x

val id1: Int => Int = x => x

soma(x => x, 1, 5)
soma(x => x * x, 1, 5)
soma(id1, 1, 5)
id1(10)

def produto(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * produto(f, a + 1, b)

def intervalo(op: (Int, Int) => Int, z: Int, f: Int => Int, a: Int, b: Int): Int =
  if (a > b) z
  else op(f(a), intervalo(op, z, f, a + 1, b))

def produto1(f: Int => Int, a: Int, b: Int) =
  intervalo((x, y) => x * y, 1, f, a, b)

def soma1(f: Int => Int, a: Int, b: Int) =
  intervalo((x, y) => x + y, 1, f, a, b)

def fat(x: Int) = produto1(x => x, 1, x)

fat(5)

def opseq[T](op: (T, T) => T, z: T,
             next: T => T,
             gt: (T, T) => Boolean,
             f: T => T, a: T, b: T): T =
  if (gt(a, b)) z
  else op(f(a), opseq(op, z, next,
    gt, f, next(a), b))

def produto2(f: Double => Double, a: Double, b: Double) = opseq[Double]((x, y) => x * y,
  1.0, x => x + 1, (x, y) => x > y, f, a, b)

def opgen[T](op: (T, T) => T, z: T,
             next: T => T,
             gt: (T, T) => Boolean)
            (f: T => T, a: T, b: T): T = {
  if (gt(a, b)) z
  else op(f(a), opgen(op, z, next, gt)
                   (f, next(a), b))
}

def produto3 =
   opgen[Double]((x, y) => x * y,
     1.0, x => x + 1, (x, y) => x > y) _

def pontoFixo(f: Double => Double)(est: Double): Double = {
  val erro = 0.0001
  def suficiente(est1: Double, est2: Double) =
    Math.abs(est2 - est1) < erro
  def loop(est: Double): Double = {
    val prox = f(est)
    if (suficiente(est, prox)) prox
    else loop(prox)
  }
  loop(est)
}

pontoFixo(x => Math.sin(x))(2)

def raiz(x: Double) = pontoFixo(y => (y + x / y)/2)(1.0)

raiz(2)
raiz(3)
raiz(10)
raiz(100)

def deriv(f: Double => Double)(x: Double) = {
  val dx = 0.0000001
  (f(x + dx) - f(x)) / dx
}

def newton(f: Double => Double) =
  pontoFixo(x => x - f(x)/deriv(f)(x))(1.0)

def raiz1(x: Double) = newton(y => y * y - x)

raiz1(2)
raiz1(3)
raiz1(10)
raiz1(100)
