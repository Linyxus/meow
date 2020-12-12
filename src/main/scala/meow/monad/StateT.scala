package meow.monad
import meow._
import Meow._

case class StateT[S, F[_], A](runStateT: S => F[(A, S)]) {
  val run: S => F[(A, S)] = runStateT

  def eval(s: S)(implicit functor: Functor[F]): F[A] = runStateT(s) <%| { case (a, _) => a }

  def exec(s: S)(implicit functor: Functor[F]): F[S] = runStateT(s) <%| { case (_, s) => s }
}

trait StateTFunctions {
  def get[S, F[_]: Monad]: StateT[S, F, S] = StateT { s => (s, s).mreturn[F] }

  def modify[S, F[_]: Monad](func: S => S): StateT[S, F, Unit] = StateT { s => ((), func(s)).mreturn[F] }

  def set[S, F[_]: Monad](value: S): StateT[S, F, Unit] = modify[S, F] { _ => value }

  def lift[S, F[_]: Monad, A](fx: F[A]): StateT[S, F, A] = StateT { s =>
    implicit val f: Functor[F] = implicitly[Monad[F]].applicative.functor
    fx <%| { x => (x, s) }
  }
}

trait StateTInstances {
  implicit def stateTIsMonad[S, F[_]: Monad]: Monad[StateT[S, F, *]] = new Monad[StateT[S, F, *]] {
    override def andThen[A, B](ma: StateT[S, F, A], fab: A => StateT[S, F, B]): StateT[S, F, B] = StateT { s1 =>
      ma.runStateT(s1) >>= { case (a, s2) => fab(a).runStateT(s2) }
    }

    override def pureOf[A](x: A): StateT[S, F, A] = StateT { s => (x, s).pure[F] }
  }
}

object StateT extends StateTFunctions with StateTInstances
