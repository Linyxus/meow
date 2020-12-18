package meow.example

import meow._
import Meow._

object ArrowExample extends App {
  val arr1: Int => Int = (x: Int) => 2 * x
  val arr2: Int => Int = (x: Int) => 10 * x
  def dup[A]: A => (A, A) = (x: A) => (x, x)
  val app: Int => Int = (arr1 &&& arr2) >>> { case (x, y) => x + y }
  println(app(10))
}
