package meow

trait Monad[F[_]] {
  def returnOf[A](x: A)(implicit applicative: Applicative[F]): F[A] = applicative.pureOf(x)

  def andThen[A, B](ma: F[A], fab: A => F[B]): F[B]
}

trait MonadInstances {
  implicit val listIsMonad: Monad[List] = new Monad[List] {
    override def andThen[A, B](ma: List[A], fab: A => List[B]): List[B] = ma flatMap fab
  }

  implicit val optionIsMonad: Monad[Option] = new Monad[Option] {
    override def andThen[A, B](ma: Option[A], fab: A => Option[B]): Option[B] = ma flatMap fab
  }

  implicit def eitherIsMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def andThen[A, B](ma: Either[E, A], fab: A => Either[E, B]): Either[E, B] = ma flatMap fab
  }
}
