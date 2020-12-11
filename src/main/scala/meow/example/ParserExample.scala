package meow.example

import meow._
import Meow._
import meow.monad._
import Parser._

object ParserExample extends App {
  val p1: Parser[Char] = char('1') >> char ('2') >> char('3')
  println(p1.runParser("1234"))
  println(p1.runParser("132"))
}
