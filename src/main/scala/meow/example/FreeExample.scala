package meow.example

import meow._
import Meow._
import meow.monad.{Free, Identity, Join, Pure, StateT}
import Free._
import StateT._
import NaturalTransformation._

object FreeExample extends App {
  sealed trait Command[+A]
  case class Inc[A](n: A) extends Command[A]
  case class Dec[A](n: A) extends Command[A]
  case class Reset[A](x: Int, n: A) extends Command[A]
  case class Out[A](f: Int => A) extends Command[A]
  object Command {
    val inc: Command[Unit] = Inc(())
    val dec: Command[Unit] = Dec(())
    def reset(x: Int): Command[Unit] = Reset(x, ())
    def output[A](f: Int => A): Command[A] = Out(f)
  }

  implicit val commandIsFunctor: Functor[Command] = new Functor[Command] {
    override def fmap[A, B](func: A => B, fx: Command[A]): Command[B] = fx match {
      case Out(f) => Out { x => func(f(x)) }
      case Inc(n) => Inc(func(n))
      case Dec(n) => Dec(func(n))
      case cmd @ Reset(x, n) => Reset(x, func(n))
    }
  }

  val evalCommand: Command ~> StateT[Int, Identity, *] = new NaturalTransformation[Command, StateT[Int, Identity, *]] {
    override def apply[A](fa: Command[A]): StateT[Int, Identity, A] = fa match {
      case Inc(n) => modify[Int, Identity](_ + 1) >> n.pure[StateT[Int, Identity, *]]
      case Dec(n) => modify[Int, Identity](_ - 1) >> n.pure[StateT[Int, Identity, *]]
      case Reset(x, n) => set[Int, Identity](x) >> n.pure[StateT[Int, Identity, *]]
      case Out(f) => get[Int, Identity] <%| f
    }
  }

  val prog1: Free[Command, Int] = Command.inc.liftFree >> Command.inc.liftFree >> Command.output(identity).liftFree
  val app1: StateT[Int, Identity, Int] = prog1.transformMonad(evalCommand)
  println(app1.eval(0).runIdentity)

  val prog2: Free[Command, Int] = Command.inc.liftFree >> Command.inc.liftFree >>
    Command.reset(0).liftFree >> Command.output(identity).liftFree
  val app2: StateT[Int, Identity, Int] = prog2.transformMonad(evalCommand)
  println(app2.eval(0).runIdentity)
}
