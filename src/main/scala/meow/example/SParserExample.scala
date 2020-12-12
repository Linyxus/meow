package meow.example

import meow._
import Meow._
import meow.monad.SParser
import SParser._

object SParserExample extends App {
  case class Bracket(items: List[Bracket])
  def pBracket: SParser[Bracket] =
    for (_ <- string("("); brackets <- many(pBracket); _ <- string(")"))
      yield Bracket(brackets)

  def printBracket(bracket: Bracket, indent: Int = 0): Unit = {
    def doIndent(): Unit = for (_ <- 0 until indent) print(' ')
    if (bracket.items.isEmpty) {
      doIndent()
      println("()")
    }
    else {
      doIndent()
      println("(")

      for (b <- bracket.items) printBracket(b, indent = indent + 1)

      doIndent()
      println(")")
    }
  }

  pBracket parse "(()(((()))()))" foreach { x => printBracket(x) }
}
