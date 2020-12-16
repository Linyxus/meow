package meow.monad

import meow.{Functor, Monad}
import meow.syntax.FunctorOpInstances

trait Free[+F[_], +A]

case class Pure[A](x: A) extends Free[Nothing, A]

case class Join[F[_]: Functor, A](ffa: F[Free[F, A]]) extends Free[F, A]

trait FreeInstances extends FunctorOpInstances {
  implicit def freeIsMonad[F[_]: Functor]: Monad[Free[F, *]] = new Monad[Free[F, *]] {
    override def andThen[A, B](ma: Free[F, A], fab: A => Free[F, B]): Free[F, B] = ma match {
      case Join(ffa) => Join(ffa <%| { fx => andThen(fx, fab) })
      case Pure(x) => fab(x)
    }

    override def pureOf[A](x: A): Free[F, A] = Pure(x)
  }
}
