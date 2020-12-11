package meow.monad

import meow._
import Meow._
import meow.monad.syntax.CanParseOpInstance

case class Parser[+A](runParser: String => List[(String, A)]) extends ParserFunctions {
  def parseOnly(source: String): Option[A] = runParser(source) filter { case (str, a) => str.isEmpty } match {
    case head :: _ => Some(head._2)
    case Nil => None
  }

  def sepBy1[B](sep: Parser[B]): Parser[List[A]] = {
    val go: Parser[List[A]] = many(sep >> this)
    val cons = (x: A, xs: List[A]) => x :: xs
    cons <%%> this <*> go
  }

  def sepBy[B](sep: Parser[B]): Parser[List[A]] = sepBy1(sep) <|> mempty[Parser, List[A]]
}

trait ParserFunctions extends ParserInstances {
  def satisfy(p: Char => Boolean): Parser[Char] = Parser { s =>
    s.headOption match {
      case Some(ch) if p(ch) => List((s.tail, ch))
      case _ => List.empty
    }
  }

  def char(ch: Char): Parser[Char] = satisfy(_ == ch)

  def string(s: String): Parser[String] =
    if (s.isEmpty) "".pure[Parser]
    else (char(s.head) >> string(s.tail)) map { _ => s }

  def choice[A](ps: Parser[A]*): Parser[A] = asum(ps.toList)
}

trait ParserInstances {
  implicit val parserIsFunctor: Functor[Parser] = new Functor[Parser] {
    override def fmap[A, B](func: A => B, fx: Parser[A]): Parser[B] = Parser { s =>
      fx.runParser(s) <%| { case (r, x) => (r, func(x)) }
    }
  }

  implicit val parserIsApplicaitve: Applicative[Parser] = new Applicative[Parser] {
    override def pureOf[A](x: A): Parser[A] = Parser { s => (s, x) :: Nil }

    override def ap[A, B](mfunc: Parser[A => B], ma: => Parser[A]): Parser[B] = Parser { s1 =>
      mfunc.runParser(s1) >>= { case (s2, func) =>
        ma.runParser(s2) <%| { case (s3, x) => (s3, func(x)) }
      }
    }
  }

  def joinParser[A](mma: Parser[Parser[A]]): Parser[A] = Parser { s1 =>
    mma.runParser(s1) >>= { case (s2, ma) => ma.runParser(s2) }
  }

  implicit val parserIsMonad: Monad[Parser] = new Monad[Parser] {
    override def andThen[A, B](ma: Parser[A], fab: A => Parser[B]): Parser[B] = joinParser(fab <%> ma)
  }

  implicit val parserIsAlternative: Alternative[Parser] = new Alternative[Parser] {
    override def or[A](fx: Parser[A], fy: => Parser[A]): Parser[A] = Parser { s =>
      fx.runParser(s) <|> fy.runParser(s)
    }

    override def mempty[A]: Parser[A] = Parser { s => Nil }
  }
}

object Parser extends ParserFunctions with ParserInstances with CanParseOpInstance
