package meow.example

import meow._
import Meow._
import meow.monad.{Free, Identity, Join, Pure, State, Writer}
import Free._
import State._
import Writer._
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

  val evalCommand: Command ~> State[Int, *] = new NaturalTransformation[Command, State[Int, *]] {
    override def apply[A](fa: Command[A]): State[Int, A] = fa match {
      case Inc(n) => modify[Int](_ + 1) >> n.pure[State[Int, *]]
      case Dec(n) => modify[Int](_ - 1) >> n.pure[State[Int, *]]
      case Reset(x, n) => set[Int](x) >> n.pure[State[Int, *]]
      case Out(f) => get[Int] <%| f
    }
  }

  val logCommand: Command ~> Writer[List[String], *] = new NaturalTransformation[Command, Writer[List[String], *]] {
    override def apply[A](fa: Command[A]): Writer[List[String], A] = fa match {
      case Inc(n) => tell("increment".pure[List]) >> n.pure[Writer[List[String], *]]
      case Dec(n) => tell("decrement".pure[List]) >> n.pure[Writer[List[String], *]]
      case Reset(x, n) => tell(s"reset to $x".pure[List]) >> n.pure[Writer[List[String], *]]
      case Out(f) => tell(s"output".pure[List]) >> f(0).pure[Writer[List[String], *]]
    }
  }

  val prog1: Free[Command, Int] = Command.inc.liftFree >> Command.inc.liftFree >> Command.output(identity).liftFree
  val app1: State[Int, Int] = prog1.transformMonad(evalCommand)
  val log1: Writer[List[String], Int] = prog1.transformMonad(logCommand)
  println(app1.eval(0))
  println(log1.run)

  val prog2: Free[Command, Int] = Command.inc.liftFree >> Command.inc.liftFree >>
    Command.reset(0).liftFree >> Command.output(identity).liftFree
  val app2: State[Int, Int] = prog2.transformMonad(evalCommand)
  val log2: Writer[List[String], Int] = prog2.transformMonad(logCommand)
  println(app2.eval(0))
  println(log2.run)
}
