package meow

trait Monad[F[_]] {
  val applicative: Applicative[F]

  def returnOf[A](x: A): F[A] = applicative.pureOf(x)

  def andThen[A, B](ma: F[A], fab: A => F[B]): F[B]
}

trait MonadInstances extends ApplicativeInstances {
  implicit val listIsMonad: Monad[List] = new Monad[List] {
    override val applicative: Applicative[List] = implicitly[Applicative[List]]

    override def andThen[A, B](ma: List[A], fab: A => List[B]): List[B] = ma flatMap fab
  }

  implicit val optionIsMonad: Monad[Option] = new Monad[Option] {
    override val applicative: Applicative[Option] = implicitly[Applicative[Option]]

    override def andThen[A, B](ma: Option[A], fab: A => Option[B]): Option[B] = ma flatMap fab
  }

  implicit def eitherIsMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override val applicative: Applicative[Either[E, *]] = implicitly[Applicative[Either[E, *]]]

    override def andThen[A, B](ma: Either[E, A], fab: A => Either[E, B]): Either[E, B] = ma flatMap fab
  }
}
