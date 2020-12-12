package meow.monad

import meow._
import Meow._
import StateT._

case class SParser[A](parser: StateT[String, Option, A]) {
  def parse(source: String): Option[A] = parser.runStateT(source) >>= { case (a, str) =>
    if (str.isEmpty) Some(a)
    else None
  }
}

trait SParserFunctions extends SParserInstances {
  def satisfy(pred: Char => Boolean): SParser[Char] = SParser {
    get[String, Option] >>= { s =>
      s.headOption match {
        case Some(ch) if pred(ch) => modify[String, Option](_.tail) >> ch.mreturn[StateT[String, Option, *]]
        case _ => lift[String, Option, Char](Option.empty[Char])
      }
    }
  }

  def char(ch: Char): SParser[Char] = satisfy(_ == ch)

  def string(s: String): SParser[String] = {
    def go(s: String): SParser[Unit] =
      if (s.isEmpty) ().mreturn[SParser]
      else char(s.head) >> go(s.tail)
    go(s) >> s.mreturn[SParser]
  }

  def choice[A](ps: SParser[A]*): SParser[A] = asum[SParser, A](ps.toList)
}

trait SParserInstances {
  implicit val sParserIsMonad: Monad[SParser] = new Monad[SParser] {
    override def andThen[A, B](ma: SParser[A], fab: A => SParser[B]): SParser[B] =
      SParser(ma.parser >>= { a => fab(a).parser })

    override def pureOf[A](x: A): SParser[A] = SParser(x.mreturn[StateT[String, Option, *]])
  }

  implicit val sParserIsAlternative: Alternative[SParser] = new Alternative[SParser] {
    override def or[A](fx: SParser[A], fy: => SParser[A]): SParser[A] = SParser { StateT { s =>
      fx.parser.runStateT(s) match {
        case Some(ret) => Some(ret)
        case None => fy.parser.runStateT(s)
      }
    } }

    override def mempty[A]: SParser[A] = SParser(StateT.lift(Option.empty))
  }
}

object SParser extends SParserFunctions with SParserInstances
