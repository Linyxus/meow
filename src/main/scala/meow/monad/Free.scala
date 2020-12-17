package meow.monad

import meow.monad.syntax.{FreeLiftOpInstances, FreeOpInstances}
import meow.{Functor, Monad}

trait Free[+F[_], +A]

case class Pure[A](x: A) extends Free[Nothing, A]

case class Join[F[_]: Functor, A](ffa: F[Free[F, A]]) extends Free[F, A]

trait FreeInstances {
  implicit def freeIsMonad[F[_]: Functor]: Monad[Free[F, *]] = new Monad[Free[F, *]] {
    override def andThen[A, B](ma: Free[F, A], fab: A => Free[F, B]): Free[F, B] = ma match {
      case Join(ffa) =>
        Join(implicitly[Functor[F]].fmap[Free[F, A], Free[F, B]]({ fx => andThen(fx, fab) }, ffa))
      case Pure(x) => fab(x)
    }

    override def pureOf[A](x: A): Free[F, A] = Pure(x)
  }
}

object Free extends FreeInstances with FreeOpInstances with FreeLiftOpInstances
