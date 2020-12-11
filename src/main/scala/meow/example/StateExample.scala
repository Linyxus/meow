package meow.example

import meow._
import Meow._
import monad._
import State._

object StateExample extends App {
  def mul2(x: Int): Int = x * 2
  def plus10(x: Int): Int = x + 10

  val app: State[Int, Int] =
    set(1) >> modify(mul2) >> modify(plus10) >> get
  println(app.run(1))

  val app2: State[Int, Int] =
    for (
      x <- get[Int]; _ <- modify((s: Int) => s + x);
      y <- get[Int]
    ) yield y
  println(app2.run(100))

  val mult: (Int, Int) => Int = (x, y) => x * y
  val app3: State[Int, Unit] =
    mult <%%> get[Int] <*> get >>= set
  println(app3.exec(-5))
}
