package meow.example

import meow._
import Meow._
import monad._

object StateExample extends App with StateInstances with StateFunctions {
  def mul2(x: Int): Int = x * 2
  def plus10(x: Int): Int = x + 10

  val app: State[Int, Int] =
    set(1) >> modify(mul2) >> modify(plus10) >> get
  println(app.runState(1))

  val app2: State[Int, Int] =
    for (
      x <- get[Int]; _ <- modify[Int](s => s + x);
      y <- get[Int]
    ) yield y
  println(app2.runState(100))
}
