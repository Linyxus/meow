package meow.example

import meow._
import Meow._

object SemigroupExample extends App {
  val l1: List[Int] = List(1, 2, 3)
  val l2: List[Int] = l1 <%| (_ * 2)
  println(l1 <> l2)
}
