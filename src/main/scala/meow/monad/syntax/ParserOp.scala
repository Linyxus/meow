package meow.monad.syntax

import meow.monad.Parser

trait ParserOp

trait CanParseOp {
  def parseOnlyWith[A](parser: Parser[A]): Option[A]
  def ~~>[A]: Parser[A] => Option[A] = parseOnlyWith[A]
}

trait CanParseOpInstance {
  implicit def toCanParseOp(source: String): CanParseOp = new CanParseOp {
    override def parseOnlyWith[A](parser: Parser[A]): Option[A] = parser parseOnly source
  }
}
