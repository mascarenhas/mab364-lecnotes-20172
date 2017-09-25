---
layout: default
title: Linguagens de Programação - Segunda Lista de Exercícios
relpath: ..
---

Segunda Lista de Exercícios
===========================

Introdução
----------

Clone ou baixe o [projeto da lista](https://github.com/mascarenhas/mab364-lecnotes/tree/lista2),
importe ele na IDE Scala (IntelliJ IDEA), e implemente as funções
correspondentes a cada questão do exercício. Depois envie
apenas o arquivo `lista2.scala` com os fontes da sua implementação até **24/09/2017**,
usando [esse link](https://www.dropbox.com/request/PQvzpc8uGu9uVUUSqpwd).

Cálculo Lambda
--------------

O *cálculo lambda* é um modelo de computação que é uma das bases da programação
funcional, e pode ser visto como uma linguagem de programação bastante simples.

Uma expressão do cálculo lambda pode ser uma variável (um nome), uma aplicação (um par de expressões),
ou uma *abstração* (um par de um nome, chamado de *parâmetro* da abstração, e uma expressão, chamada de seu *corpo*).
As abstrações também são os únicos valores da linguagem (isso mesmo, o cálculo lambda não tem números ou booleanos).

### Questão 1

Defina um tipo algébrico para expressões no cálculo lambda, usando um trait `CL` e três construtores
`Var`, `Ap`, e `Abs`.

### Questão 2	
	
A semântica do cálculo lambda call-by-value é dada por substituição: o valor de uma variável é
indefinido; o valor de uma abstração é ela mesma; para obter o valor de uma aplicação obtemos
o valor do seu lado esquerdo (que deverá ser uma abstração, ou a aplicação é indefinida)
e seu lado direito, substituímos o parâmetro do lado esquerdo pelo
valor do lado direito no corpo do lado esquerdo, e achamos o valor do resultado.

Para evitar capturas indevidas, a substituição deve renomear variáveis livres do mesmo jeito
que a substituição de `fun`.

Escreva a função que acha as variáveis livres `fv(e: CL): Set[String]`, a
função de substituição `subst(oque: String, peloque: CL, onde: CL)`, 
o interpretador big-step `eval(e: CL): Abs` e o interpretador small-step `step(e: CL): CL`.

### Questão 3

Escreva as funções `eval_cbn` e `step_cbn` para o interpretador call-by-name do 
cálculo lambda.
	
* * * * *

Última Atualização: {{ site.time | date: "%Y-%m-%d %H:%M" }}

