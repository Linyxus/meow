package meow.example

import meow._
import Meow._
import monad.Writer
import Writer._

object WriterExample extends App {
  val app: Writer[String, Unit] =
    tell("Hello, world;") >> 0.pure[Writer[String, *]] >>= { x =>
      if (x >= 0) tell("x >= 0")
      else tell("x < 0")
    }

  println(app.exec)
}
