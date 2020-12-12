package meow.example

import meow._
import Meow._
import meow.monad.OptionT
import OptionT._
import meow.monad.StateT
import StateT._
import meow.monad.Identity
import Identity._

object OptionTExample extends App {
  val app: OptionT[StateT[Int, Identity, *], Int] =
    OptionT.lift(get[Int, Identity]) >>= { x => if (x > 0) fail else OptionT.lift(get[Int, Identity]) }
  println(app.runOptionT.eval(10))
  println(app.runOptionT.eval(-10))
}
