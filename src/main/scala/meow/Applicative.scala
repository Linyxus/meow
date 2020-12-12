package meow

trait Applicative[F[_]] {
  val functor: Functor[F]
  def pureOf[A](x: A): F[A]
  def ap[A, B](mfunc: F[A => B], ma: => F[A]): F[B]
}

trait ApplicativeInstances extends FunctorInstances {
  implicit val listIsApplicative: Applicative[List] = new Applicative[List] {
    override val functor: Functor[List] = implicitly[Functor[List]]
    override def pureOf[A](x: A): List[A] = x :: Nil

    override def ap[A, B](mfunc: List[A => B], ma: => List[A]): List[B] =
      for (func <- mfunc; a <- ma) yield func(a)
  }

  implicit val optionIsApplicative: Applicative[Option] = new Applicative[Option] {
    override val functor: Functor[Option] = implicitly[Functor[Option]]

    override def pureOf[A](x: A): Option[A] = Some(x)

    override def ap[A, B](mfunc: Option[A => B], ma: => Option[A]): Option[B] =
      for (func <- mfunc; a <- ma) yield func(a)
  }

  implicit def eitherIsApplicative[E]: Applicative[Either[E, *]] = new Applicative[Either[E, *]] {
    override val functor: Functor[Either[E, *]] = implicitly[Functor[Either[E, *]]]

    override def pureOf[A](x: A): Either[E, A] = Right(x)

    override def ap[A, B](mfunc: Either[E, A => B], ma: => Either[E, A]): Either[E, B] =
      (mfunc, ma) match {
        case (Right(func), Right(a)) => Right(func(a))
        case (Right(_), Left(e)) => Left(e)
        case (Left(e), _) => Left(e)
      }
  }
}
