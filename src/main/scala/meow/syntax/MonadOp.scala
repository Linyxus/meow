package meow.syntax

import meow.{Applicative, Monad}

trait MonadOp[F[_], A] {
  val monad: Monad[F]

  def >>=[B](fab: A => F[B]): F[B]

  def >>[B](mb: F[B]): F[B] = this.>>= { _ => mb }

  def flatMap[B](fab: A => F[B]): F[B] = this.>>=(fab)
}

trait MonadExtraOp[F[_], A] {
  def <*[B](mb: F[B])(implicit applicative: Applicative[F]): F[A]
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

trait MonadExtraOpInstances extends MonadOpInstances with MonadReturnOpInstances {
  implicit def toMonadExtraOp[F[_]: Monad, A](ma: F[A]): MonadExtraOp[F, A] = new MonadExtraOp[F, A] {
    override def <*[B](mb: F[B])(implicit applicative: Applicative[F]): F[A] =
      ma >>= { a => mb >> a.mreturn[F] }
  }
}

