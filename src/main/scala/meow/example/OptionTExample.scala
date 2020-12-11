package meow.example

import meow._
import Meow._
import meow.monad.OptionT
import OptionT._
import meow.monad.State
import State._

object OptionTExample extends App {
  val app: OptionT[State[Int, *], Int] =
    lift(get[Int]) >>= { x => if (x > 0) fail[State[Int, *], Int] else lift(get[Int]) }
  println(app.runOptionT.eval(10))
  println(app.runOptionT.eval(-10))
}
