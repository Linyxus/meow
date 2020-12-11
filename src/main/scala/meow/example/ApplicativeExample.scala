package meow.example

import meow._
import Meow._

object ApplicativeExample extends App {
  def add(x: Int, y: Int): Int = x + y
  val foo: Int => Int => Int = curry2(add)

  println(foo <%> List(1, 2) <*> List(3, 4))
  println(add _ <%%> List(1, 2) <*> List(3, 4))

  val m1: List[Int] = 1.pure[List]
  println(m1)

  val m2: Option[Int] = 1.pure[Option]
  println(m2)

  val m3: Either[Nothing, Int] = 1.pure[Either[Nothing, *]]
  println(m3)
}
