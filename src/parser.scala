import scala.io.Source

package object parser {
  type Parser[+A] = (Vector[Char], Int, Int, Set[String]) => (Option[(A, Int)], Int, Set[String])

  def posToLineCol(input: String, pos: Int) = {
    val line = input.substring(0, pos).count(c => c == '\n') + 1
    val column = pos - input.lastIndexWhere(c => c == '\n', pos)
    (line, column)
  }

  def syntaxError(input: String, pos: Int, exp: Set[String]) = {
    val (lin, col) = posToLineCol(input, pos)
    val part = input.substring(pos, pos + 10)
    sys.error(s"erro de sintaxe na linha $lin e coluna $col ($part), esperado: ${exp.mkString(", ")}")
  }

  implicit class RichParser[+A](val p: Parser[A]) extends AnyVal {
    def flatMap[B](f: A => Parser[B]): Parser[B] = bind(p, f)
    def map[B](f: A => B): Parser[B] = adapt(p, f)
    def filter(f: A => Boolean): Parser[A] = constrain(p, f)
    def withFilter(f: A => Boolean): Parser[A] = constrain(p, f)

    def parse(input: String): A = {
      val (res, fail, exp) = p(input.toVector, 0, -1, Set())
      res match {
        case None => syntaxError(input, fail, exp)
        case Some((a, pos)) => a
      }
    }

    def parseFile(name: String): A = {
      val input = Source.fromFile(name).getLines().mkString("\n")
      parse(input)
    }

    def +:[B >: A](op: Parser[B]): Parser[B] = choice(op, p)
    def *:[B](op: Parser[B]): Parser[Unit] = for {
      _ <- op
      _ <- p
    } yield ()
  }

  def empty[A](v: A): Parser[A] =
    (input, pos, fail, exp) => (Some((v, pos)), fail, exp)

  def pred(pred: Char => Boolean): Parser[Char] =
    (input, pos, fail, exp) =>
      if(input.isDefinedAt(pos) && pred(input(pos)))
        (Some((input(pos), pos+1)), fail, exp)
      else
        (None, fail, exp)

  def choice[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
    (input, pos, fail, exp) => {
      val (res, nfail, nexp) = p1(input, pos, fail, exp)
      res match {
        case None => p2(input, pos, nfail, nexp)
        case Some(_) => (res, nfail, nexp)
      }
    }

  def bind[A,B](p: Parser[A], f: A => Parser[B]): Parser[B] =
    (input, pos, fail, exp) => {
      val (res, nfail, nexp) = p(input, pos, fail, exp)
      res match {
        case None => (None, nfail, nexp)
        case Some((a, npos)) => f(a)(input, npos, nfail, nexp)
      }
    }

  def adapt[A,B](p: Parser[A], f: A => B): Parser[B] =
    (input, pos, fail, exp) => {
      val (res, nfail, nexp) = p(input, pos, fail, exp)
      res match {
        case None => (None, nfail, nexp)
        case Some((a, npos)) => (Some((f(a), npos)), nfail, nexp)
      }
    }

  def constrain[A](p: Parser[A], f: A => Boolean): Parser[A] =
    (input, pos, fail, exp) => {
      val (res, nfail, nexp) = p(input, pos, fail, exp)
      res match {
        case None => (None, nfail, nexp)
        case Some((a, npos)) => if(f(a)) (res, nfail, nexp)
        else (None, nfail, nexp)
      }
    }

  def not[A,B](p: Parser[A], v: B): Parser[B] =
    (input, pos, fail, exp) => {
      val (res, _, _) = p(input, pos, fail, exp)
      res match {
        case None => (Some((v, pos)), fail, exp)
        case Some(_) => (None, fail, exp)
      }
    }

  def expect[A](p: Parser[A], name: String): Parser[A] =
    (input, pos, fail, exp) => {
      val (res, nfail, nexp) = p(input, pos, fail, exp)
      res match {
        case None => if(pos == fail) (None, fail, exp + name)
        else if(pos > fail) (None, pos, Set(name))
        else (None, fail, exp)
        case Some(_) => (res, nfail, nexp)
      }
    }

  val pos: Parser[Int] =
    (input, pos, fail, exp) => (Some((pos, pos)), fail, exp)

  def term(c: Char): Parser[Char] = pred(oc => oc == c)

  def lift[A](ps: List[Parser[A]]): Parser[List[A]] = ps match {
    case Nil => empty(List())
    case ph :: pt => for {
      h <- ph
      t <- lift(pt)
    } yield h :: t
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    (for {
      h <- p
      t <- many(p)
    } yield h :: t) +: empty(List())

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    (for { a <- p } yield Some(a)) +: empty(None)

  def discard[A](p: Parser[A]): Parser[Unit] =
    for { _ <- p } yield ()

  // pl { pop pr } - ex. E -> E { + T }
  def chainl[A](pl: Parser[A], pop: Parser[(A, A) => A], pr: Parser[A]): Parser[A] =
    for {
      z <- pl
      l <- many(for {
        op <- pop
        r <- pr
      } yield (op, r))
    } yield l.foldLeft(z)({ case (l, (op, r)) => op(l, r) })

  val space: Parser[Unit] =
    discard(many((term('-') *: term('-') *: many(pred(c => c != '\n'))) +:
      discard(pred(c => c.isWhitespace))))

  def kw(s: String): Parser[(String, Int)] = for {
    _ <- space
    pos <- pos
    _ <- expect(lift(s.toList.map(c => term(c))) *: not(pred(c => c.isUnicodeIdentifierPart), ()), s)
  } yield (s, pos)

  val id: Parser[(String, Int)] = for {
    _ <- space
    pos <- pos
    res <- expect(for {
      h <- pred(c => c.isUnicodeIdentifierPart && !c.isDigit)
      t <- many(pred(c => c.isUnicodeIdentifierPart))
    } yield ((h :: t).mkString, pos), "identifer")
  } yield res

  val num: Parser[(String, Int)] = for {
    _ <- space
    pos <- pos
    res <- expect(for {
      h <- pred(c => c.isDigit)
      t <- many(pred(c => c.isDigit))
      dec <- (for {
        dot <- term('.')
        h <- pred(c => c.isDigit)
        t <- many(pred(c => c.isDigit))
      } yield (dot :: h :: t).mkString) +: empty("")
    } yield ((h :: t).mkString + dec, pos), "numeral")
  } yield res

  val str: Parser[(String, Int)] = for {
    _ <- space
    pos <- pos
    res <- expect(for {
      _ <- pred(c => c == '"')
      s <- many(pred(c => c != '"'))
      _ <- pred(c => c == '"')
    } yield (s.mkString, pos), "string")
  } yield res

  def op(s: String): Parser[(String, Int)] = for {
    _ <- space
    pos <- pos
    _ <- expect(lift(s.toList.map(c => term(c))), s)
  } yield (s, pos)
}