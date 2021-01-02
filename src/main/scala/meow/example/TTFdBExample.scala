package meow.example

import meow.toys.TTFdB
import TTFdB._

object TTFdBExample extends App {
  // 1 + 2
  def prog1[Repr[_, _] : Symantics, Env]: Repr[Env, Int] = {
    val f: Symantics[Repr] = implicitly
    f.add(f.int(1), f.int(2))
  }

  println(show[Unit, Int](prog1[Show, Unit]))
  println(eval[Unit, Int](prog1[Eval, Unit], ()))

  // (\x. 1 + x) 2
  def prog2[Repr[_, _] : Symantics, Env]: Repr[Env, Int] = {
    val f: Symantics[Repr] = implicitly
    f.app(f.lam(f.add(f.int[(Int, Env)](1), f.here[Env, Int])), f.int(2))
  }

  println(show[Unit, Int](prog2[Show, Unit]))
  println(eval[Unit, Int](prog2[Eval, Unit], ()))
}
