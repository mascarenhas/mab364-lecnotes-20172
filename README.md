---
layout: default
title: Linguagens de Programação - Terceira Lista de Exercícios
relpath: ..
---

Terceira Lista de Exercícios
============================

Introdução
----------

Clone ou baixe o [projeto da lista](https://github.com/mascarenhas/mab364-lecnotes/tree/lista3),
importe ele no IntelliJ, e implemente as funções
correspondentes a cada questão do exercício. Depois envie um novo `Lista3.zip` com
as modificações até **30/10/2017**,
usando [esse link](https://www.dropbox.com/request/X918U9PG1nEH8L6Xrepr).

Questão 1 - uma biblioteca padrão para *fun*
--------------------------------------------

Implemente os padrões de recursão em listas que usamos em Scala: `map`, `filter`,
`foldRight`, `foldLeft` e `flatMap` como funções de *fun* para trabalhar com listas
construídas com as funções abaixo:

    fun Cons(h, t)
      fun (v, c)
        (c)(h, t)
      end
    end
    
    fun Vazia()
      fun (v, c)
        (v)()
      end
    end
    
	fun tamanho(l)
	  (l)(fun () 0 end, fun (h, t) 1 + tamanho(t) end)
	end
	
    fun map(f, l)
      -- implementação de map
    end
    
    fun filter(p, l)
      -- implementação de filter
    end
    
    fun foldLeft(z, op, l)
      -- implementação de foldLeft
    end
    
    fun foldRight(z, op, l)
      -- implementação de foldRight
    end
    
    fun flatMap(f, l)
      -- implementação de flatMap
    end
	
Implemente as funções no arquivo `listas.sc`.
	
Questão 2 - while
-----------------

Embora possamos usar recursão para fazer laços, em uma linguagem imperativa o mais
natural é ter uma expressão específica para isso. Vamos acrescentar um laço `while`
a fun, com a sintaxe `while EXP do EXP end`. O parser de fun já foi modificado
para aceitar essa expressão e gerar um nó `While` com a estrutura abaixo:

    case class While(econd: Exp, corpo: Exp) extends Exp	

Acrescente casos a `eval`, `fvs` e `subst` para `While`.
	
Questão 3 - entrada e saída
---------------------------

Um tipo bem comum de efeito colateral é a entrada e saída, onde uma linguagem
de programação pode ler valores de algum dispositivo de entrada e escrever valores
para algum dispositivo de saída.

Podemos incorporar entrada e saída a fun modificando o tipo `Acao[T]` para receber
e produzir "dispositivos" de entrada e saída:

    type Acao[T] = (Stream[Valor], List[String], Mem) => (Either[T, Erro], Stream[Valor], List[String], Mem)

A entrada é um `Stream` de valores. Uma stream é uma lista potencialmente infinita: dada uma stream `s` lemos
o próximo elemento dela com `s.head`, e obtemos uma stream sem esse próximo elemento com `s.tail`. A saída
é a lista de strings impressas.
	
Podemos acrescentar duas ações primitivas, uma para ler um valor da entrada e outra
para imprimir uma string na saída:

    def levalor: Acao[Valor] = ???
	def imprime(s: Valor): Acao[Valor] = ???

Implemente a mudança no tipo `Acao[T]`, mudando as primitivas existentes para levar em
conta entrada/saída (em especial a `bind`), depois implemente as novas primitivas.
Preste atenção na linearidade da entrada/saída.

Para usar essas primitivas precisamos de novas expressões em fun. O parser de fun
já foi modificado para aceitar as expressões `read` e `print '(' [EXP {',' EXP}] ')'`,
que correspondem aos casos abaixo:

    case class Read() extends Exp
	case class Print(args: List[Exp]) extends Exp

Implemente os casos de `Read` e `Print` nas funções `fvs`, `subst`, e `eval`. A
expressão `Read` avalia para o próximo valor na entrada (consumindo esse valor),
enquanto `Print` avalia sua lista de argumentos da esquerda pra direita e os
imprime como uma string, separados por tabs.	
		
* * * * *

Última Atualização: {{ site.time | date: "%Y-%m-%d %H:%M" }}

