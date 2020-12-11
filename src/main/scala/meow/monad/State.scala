package meow.monad

import meow.syntax.{CanFMapOpInstances, FunctorOpInstances, MonadOpInstances, MonadReturnOpInstances}
import meow.{Applicative, Functor, Monad}

case class State[S, +A](runState: S => (A, S)) {
  def exec(s: S): S = runState(s)._2
  def eval(s: S): A = runState(s)._1
  val run: S => (A, S) = runState
}

object State extends StateFunctions with StateInstances

trait StateFunctions
  extends MonadOpInstances with MonadReturnOpInstances with StateInstances with FunctorOpInstances {
  def get[S]: State[S, S] = State { s => (s, s) }
  def set[S](s: S): State[S, Unit] = State { _ => ((), s) }
  def modify[S](func: S => S): State[S, Unit] = get[S] <%| func >>= set
}

trait StateInstances extends CanFMapOpInstances {
  implicit def stateIsFunctor[S]: Functor[({type P[A] = State[S, A]})#P] = new Functor[({type P[A] = State[S, A]})#P] {
    override def fmap[A, B](func: A => B, fx: State[S, A]): State[S, B] = State { state =>
      val ret = fx.runState(state)
      (func(ret._1), ret._2)
    }
  }

  implicit def stateIsApplicative[S]: Applicative[({type P[A] = State[S, A]})#P] = new Applicative[({type P[A] = State[S, A]})#P] {
    override def pureOf[A](x: A): State[S, A] = State { s => (x, s) }

    override def ap[A, B](mfunc: State[S, A => B], ma: State[S, A]): State[S, B] = State { s1 =>
      val (f, s2) = mfunc.runState(s1)
      val (a, s3) = ma.runState(s2)
      (f(a), s3)
    }
  }

  def joinState[S, A](state: State[S, State[S, A]]): State[S, A] = State { s1 =>
    val (ma, s2) = state.runState(s1)
    ma.runState(s2)
  }

  implicit def stateIsMonad[S]: Monad[({type P[A] = State[S, A]})#P] = new Monad[({type P[A] = State[S, A]})#P] {
    override def andThen[A, B](ma: State[S, A], fab: A => State[S, B]): State[S, B] = joinState(fab <%> ma)
  }
}
