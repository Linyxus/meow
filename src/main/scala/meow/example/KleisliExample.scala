package meow.example

import meow._
import Meow._
import monad.Writer
import Writer._

object KleisliExample extends App {
  def liftK[A, B](func: A => B): Kleisli[Writer[List[String], *], A, B] = Kleisli { x =>
    val y = func(x)
    tell(s"change from $x to $y".pure[List]) >> y.pure[Writer[List[String], *]]
  }
  val app: Kleisli[Writer[List[String], *], Int, Int] =
    (liftK[Int, Int](_ * 2) &&& liftK[Int, Int](_ + 1)) >>> liftK[(Int, Int), Int] { case (a, b) => a * b }
  println(app.runKleisli(10).run)
}
