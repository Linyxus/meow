package meow.example

import meow._
import Meow._
import meow.monad.StateT
import StateT._

object StateTExample extends App {
  val app: StateT[Int, Option, String] =
    get[Int, Option] >>= { x =>
      if (x > 5)
        lift(Option.empty)
      else
        "Big number!".mreturn[StateT[Int, Option, *]]
    }

  println(app.runStateT(5))
  println(app.runStateT(10))
}
