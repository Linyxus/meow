package meow.example

import meow._
import Meow._

object AlternativeExample extends App {
  println(List(1, 2) <|> List(3, 4))

  val m1: Option[Int] = Some(1)
  val m2: Option[Int] = Some(2)
  val m3: Option[Int] = None
  println(m1 <|> m2)
  println(m2 <|> m1)
  println(m1 <|> m3)
  println(m3 <|> m1)
  println(m3 <|> m3)
}
