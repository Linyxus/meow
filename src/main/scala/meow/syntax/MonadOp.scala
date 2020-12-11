package meow.syntax

import meow.{Applicative, Monad}

trait MonadOp[F[_], A] {
  val monad: Monad[F]

  def >>=[B](fab: A => F[B]): F[B]
}

trait MonadReturnOp[A] {
  def mreturn[F[_]: Monad](implicit applicative: Applicative[F]): F[A]
}

trait MonadOpInstances {
  implicit def toMonadOp[F[_]: Monad, A](ma: F[A]): MonadOp[F, A] = new MonadOp[F, A] {
    override val monad: Monad[F] = implicitly[Monad[F]]

    override def >>=[B](fab: A => F[B]): F[B] = monad.andThen(ma, fab)
  }
}

trait MonadReturnOpInstances {
  implicit def toMonadReturnOp[A](x: A): MonadReturnOp[A] = new MonadReturnOp[A] {
    override def mreturn[F[_] : Monad](implicit applicative: Applicative[F]): F[A] = {
      val monad: Monad[F] = implicitly[Monad[F]]
      monad.returnOf(x)
    }
  }
}
