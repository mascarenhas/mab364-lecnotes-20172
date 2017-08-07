import scala.annotation.tailrec

10 + 2 * 5 - 3
10 + 2 * 5
2 * 5

val pi = 3.14159
val raio = 10
2 * pi * raio

def circunferencia(raio: Double) =
  2 * pi * raio

def quadrado(x: Double): Double = x * x

circunferencia(5)

def somaDeQuadrados(a: Double, b: Double) =
  quadrado(a) + quadrado(b)

somaDeQuadrados(3, 2 + 2)
somaDeQuadrados(3, 4)
quadrado(3) + quadrado(4)
3 * 3 + quadrado(4)
9 + quadrado(4)
9 + 4 * 4
9 + 16
25

somaDeQuadrados(3, 2 + 2)
quadrado(3) + quadrado(2 + 2)
3 * 3 + quadrado(2 + 2)
9 + quadrado(2 + 2)
9 + (2 + 2) * (2 + 2)
9 + 4 * (2 + 2)
9 + 4 * 4
9 + 16
25

def loop: Double = loop
def primeiro(x: Double, y: => Double) = x

primeiro(1, loop)
1

def loopB: Boolean = loopB

true || loopB
false && loopB

def abs(x: Double) = if (x < 0) -x else x

def raiz(x: Double) = {
  def raizIter(est: Double, x: Double): Double =
    if (suficiente(est, x)) est else raizIter(melhora(est, x), x)

  def suficiente(est: Double, x: Double) =
    abs(quadrado(est) - x) < 0.001

  def melhora(est: Double, x: Double) = (est + x / est) / 2

  raizIter(1, x)
}

raiz(2)
raiz(10)
raiz(3)
raiz(100)

val x = 0
def f(y: Int) = y + 1
val result = {
  val x = f(3)
  x * x
}
x

@tailrec
def mdc(a: Int, b: Int): Int = if (b == 0) a else mdc(b, a % b)
def fat(x: Int): Int = if (x < 2) 1 else x * fat(x - 1)

mdc(14, 21)
if (21 == 0) 14 else mdc(21, 14 % 21)
if (false) 14 else mdc(21, 14 % 21)
mdc(21, 14 % 21)
mdc(21, 14 % 21)
mdc(21, 14)
if (14 == 0) 21 else mdc(14, 21 % 14)
if (false) 21 else mdc(14, 21 % 14)
mdc(14, 21 % 14)
mdc(14, 7)
if (7 == 0) 14 else mdc(7, 14 % 7)
if (false) 14 else mdc(7, 14 % 7)
mdc(7, 14 % 7)
mdc(7, 0)
if (0 == 0) 7 else mdc(0, 7 % 0)
if (true) 7 else mdc(0, 7 % 0)
7

fat(4)
if (4 < 2) 1 else 4 * fat(4 - 1)
if (false) 1 else 4 * fat(4 - 1)
4 * fat(4 - 1)
4 * fat(3)
4 * (if (3 < 2) 1 else 3 * fat(3 - 1))
4 * (if (false) 1 else 3 * fat(3 - 1))
4 * 3 * fat(3 - 1)
4 * 3 * fat(2)
4 * 3 * (if (2 < 2) 1 else 2 * fat(2 - 1))
4 * 3 * (if (false) 1 else 2 * fat(2 - 1))
4 * 3 * 2 * fat(2 - 1)
4 * 3 * 2 * fat(1)
4 * 3 * 2 * (if (1 < 2) 1 else 1 * fat(1 - 1))
4 * 3 * 2 * (if (true) 1 else 1 * fat(1 - 1))
4 * 3 * 2 * 1
4 * 3 * 2
4 * 6
24

def fatIter(x: Double, acc: Double): Double =
  if(x < 2) acc else fatIter(x - 1, x * acc)

fatIter(4, 1)
if(4 < 2) 1 else fatIter(4 - 1, 4 * 1)
if(false) 1 else fatIter(4 - 1, 4 * 1)
fatIter(4 - 1, 4 * 1)
fatIter(3, 4)
if(3 < 2) 4 else fatIter(3 - 1, 3 * 4)
if(false) 4 else fatIter(3 - 1, 3 * 4)
fatIter(3 - 1, 3 * 4)
fatIter(2, 12)
if(2 < 2) 12 else fatIter(2 - 1, 2 * 12)
if(false) 12 else fatIter(2 - 1, 2 * 12)
fatIter(2 - 1, 2 * 12)
fatIter(1, 24)
if(1 < 2) 24 else fatIter(1 - 1, 1 * 24)
if(true) 24 else fatIter(1 - 1, 1 * 24)
24

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

