10 + 2 * 5 - 3
10 + 2 * 5
2 * 5

def pi = 3.14159
def raio = 10
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
