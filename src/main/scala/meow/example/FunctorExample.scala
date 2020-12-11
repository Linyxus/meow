package meow.example

import meow._
import Meow._

object FunctorExample extends App {
  println(List(1, 2, 3) <#> (_ + 1))
  println(((x: Int) => x + 1) <%> List(1, 2, 3))

  println(None <#> ((x: Int) => x + 1))
  val m1: Option[Int] = Some(1)
  println(m1 <#> (_ + 1))
}
