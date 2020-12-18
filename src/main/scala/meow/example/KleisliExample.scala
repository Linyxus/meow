package meow.example

import meow._
import Meow._
import monad.Writer
import Writer._

object KleisliExample extends App {
  type App[A, B] = Kleisli[Writer[List[String], *], A, B]
  def liftK[A, B](func: A => B): App[A, B] = Kleisli { x =>
    val y = func(x)
    tell(s"change from $x to $y".pure[List]) >> y.pure[Writer[List[String], *]]
  }
  val app: App[Int, Int] =
    (liftK[Int, Int](_ * 2) &&& liftK(_ + 1)) >>> liftK { case (a, b) => a * b }
  println(app.runKleisli(10).run)
}
