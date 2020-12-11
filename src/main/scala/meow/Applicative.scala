package meow

trait Applicative[F[_]] {
  def pureOf[A](x: A): F[A]
  def ap[A, B](mfunc: F[A => B], ma: => F[A]): F[B]
}

trait ApplicativeInstances {
  implicit val listIsApplicative: Applicative[List] = new Applicative[List] {
    override def pureOf[A](x: A): List[A] = x :: Nil

    override def ap[A, B](mfunc: List[A => B], ma: => List[A]): List[B] =
      for (func <- mfunc; a <- ma) yield func(a)
  }

  implicit val optionIsApplicative: Applicative[Option] = new Applicative[Option] {
    override def pureOf[A](x: A): Option[A] = Some(x)

    override def ap[A, B](mfunc: Option[A => B], ma: => Option[A]): Option[B] =
      for (func <- mfunc; a <- ma) yield func(a)
  }

  implicit def eitherIsApplicative[E]: Applicative[({type L[A] = Either[E, A]})#L] = new Applicative[({type L[A] = Either[E, A]})#L] {
    override def pureOf[A](x: A): Either[E, A] = Right(x)

    override def ap[A, B](mfunc: Either[E, A => B], ma: => Either[E, A]): Either[E, B] =
      (mfunc, ma) match {
        case (Right(func), Right(a)) => Right(func(a))
        case (Right(_), Left(e)) => Left(e)
        case (Left(e), _) => Left(e)
      }
  }
}
