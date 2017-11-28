Quarta Lista de Exercícios
==========================

Introdução
----------

Baixe ou clone o [projeto da lista](https://github.com/mascarenhas/mab364-lecnotes/tree/lista4), 
importe ele no IntelliJ, e faça o que foi pedido. Depois envie um novo `Lista5.zip` com
as modificações até **12/12/2017**,
usando [esse link](https://www.dropbox.com/request/zBIHPjxZZAZOKaywIWTM).

Questão 1 - Ambientes e escopo
------------------------------

O tipo algébrico abaixo dá a sintaxe abstrata de uma linguagem funcional (chamada *dyn*) que tem variáveis
com escopo estático e variáveis com escopo dinâmico, onde variáveis introduzidas por `Let`
e parâmetros têm escopo estático
e variáveis introduzias por `LetDyn` têm escopo dinâmico. 
Os valores da linguagem são números inteiros e funções, dados por um tipo algébrico `Valor`.
A linguagem não tem efeitos colaterais.

    trait Exp
    case class Num(n: Int) extends Exp
    case class Soma(e1: Exp, e2: Exp) extends Exp
    case class Mult(e1: Exp, e2: Exp) extends Exp
    // Testa se a condição é igual a 0
    case class If0(cond: Exp, ethen: Exp, eelse: Exp) extends Exp
	// Funções têm apenas um parâmetro
    case class Proto(param: String, corpo: Exp) extends Exp
    case class Var(nome: String) extends Exp
    case class Ap(fun: Exp, arg: Exp) extends Exp
    case class Let(nome: String, exp: Exp, corpo: Exp) extends Exp
    case class LetDyn(nome: String, exp: Exp, corpo: Exp) extends Exp

Para essa questão, ponha todas as suas respostas no arquivo `dyn.scala`.	
	
* Implemente o tipo algébrico `Valor`.
* Implemente a função `eval` dessa linguagem, usando uma semântica big-step de ambientes.
Um nó `Var` é uma variável com escopo estático caso exista uma em escopo, senão é uma
variável com escopo dinâmico (em outras palavras, variáveis estáticas ocultam variáveis dinâmicas).
* Nessa linguagem, uma expressão `letrec` ainda seria necessária para definir
funções recursivas? E recursão mútua? Justifique.

Questão 2 - Continuações
------------------------

Corotinas simétricas são corotinas em que só existe a operação `resume`, rebatizada de `transfer`.
Essa operação suspende a corotina atual e começa a executar a outra corotina a partir do
ponto onde ela chamou `transfer` pela última vez (ou do início). Uma corotina chegar ao final
sem transferir o controle para outra corotina encerra o programa, e o resultado dessa corotina
é o resultado do programa todo.

Faça as modificações necessárias em *MicroC* no arquivo `microc.scala` para implementar corotinas
simétricas ao invés dos geradores atuais. A primitiva `coro` ainda instancia uma nova corotina a partir de uma
função que dá o "corpo" dessa corotina. Uma nova primitiva `transfer(e)` avalia a expressão
`e`, e transfere o controle para a corotina referenciada pelo resultado. A corotina principal
do programa tem a referência `0`. Demarque duas modificações com comentários no código.
Você pode remover o suporte a corotinas assimétricas (`resume` e `yield` do interpretador atual).

Questão 3 - Objetos
-------------------

Objetos e funções de primeira classe são dois lados da mesma moeda. Mesmo a
*recursão aberta* dos objetos pode ser simulada com funções de primeira classe.
Por exemplo, o código abaixo mostra uma versão "aberta" da função fatorial, em *fun*:

    let fat = fun (f, n)
                if n < 2 then
                  1
                else
                  n * (f)(f, n-1)
                end
              end
    in
      fat(fat, 5)
    end

Para a resposta dessa questão use o arquivo `questao3.sc`.	
	
* Escreva uma função `doubler` que recebe uma função "aberta" como a função
acima e retorna outra função aberta que chama a função passada e dobra o
resultado. Se substituirmos o corpo do `let` acima pela expressão abaixo o
resultado do programa deve ser 3840.

    let df = doubler(fat) in
      df(df, 5)
    end
* Escreva `doubler` como uma função de objetos para objetos em *proto*.
Assuma que a função que `doubler` recebe é um objeto com um método `apply`.
* Escreva a implementação recursiva tradicional da função `fib(n)` que
calcula o n-ésimo número de Fibonacci como uma função "aberta" de *fun* e como
um objeto de *proto*. Teste que ela dá os mesmos resultados nas duas linguagens,
tanto aplicada normalmente quanto usada com `doubler`.

	
* * * * *

Última Atualização: {{ site.time | date: "%Y-%m-%d %H:%M" }}

