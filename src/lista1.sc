import lista1._

quicksort(List(3, 3, 3))
quicksortH(List(3, 3, 3))

val sem1 = List((9.5,3),(7.3,4),(5.0,3),(4.0,4))

(9.5 * 3 + 7.3 * 4 + 5 * 3 + 4 * 4) /
  (3 + 4 + 3 + 4)

crSemestre(sem1)

crsAcumulados(List(sem1, sem1, sem1))

contem(unitario(2), 3)
contem(unitario(2), 2)

contem(uniao(unitario(2),
  unitario(3)), 3)

contem(uniao(uniao(uniao(unitario(2),
  unitario(3)), unitario(4)),
  unitario(5)), 6)

val pares: Conjunto[Int] = (x: Int) => x % 2 == 0

contem(pares, 4)
contem(pares, 7)

val quadrados: Conjunto[Int] = (x: Int) => Math.sqrt(x).round == Math.sqrt(x)

val paresQuadrados: Conjunto[Int] =
  intersecao(pares, quadrados)

contem(paresQuadrados, 4)
contem(paresQuadrados, 20)
contem(paresQuadrados, 9)
contem(pares, 20)
contem(quadrados, 9)

val paresStr: Conjunto[String] = map(pares,
  (s: String) => s.toInt)

contem(paresStr, "4")
contem(paresStr, "9")

for {
  x <- ConjVazio().insere(2).insere(1).insere(5)
  y <- ConjVazio().insere(4).insere(3)
} yield x + y

val x = ConjVazio().insere(2).insere(4).insere(5)
val y = ConjVazio().insere(4).insere(3)

intersecao(x, y)