package meow.example

import meow._
import Meow._

object MonadExample extends App {
  val m1: List[Int] = List(1, 2, 3, 4, 5)
  println(m1 >>= { x =>
    List(x, x * x, x * x * x)
  })

  val m2: Option[Int] = None
  println(m2 >>= { x => Some(x + 1) })
  val m3: Option[Int] = Some(1)
  println(m3 >>= { x => Some(x + 1) })
  println(m3 >>= { _ => None })

  val m5: List[Int] = 1.mreturn[List]
  println(m5)
  val m6: Option[Int] = 1.mreturn[Option]
  println(m6)
}
