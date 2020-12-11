package meow

trait Functor[F[_]] {
  def fmap[A, B](func: A => B, fx: F[A]): F[B]
}

trait FunctorInstances {
  implicit val listIsFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](func: A => B, fx: List[A]): List[B] = fx map func
  }

  implicit val optionIsFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](func: A => B, fx: Option[A]): Option[B] = fx map func
  }

  implicit def eitherIsFunctor[E]: Functor[({type P[B] = Either[E, B]})#P] = new Functor[({type P[B] = Either[E, B]})#P] {
    override def fmap[A, B](func: A => B, fx: Either[E, A]): Either[E, B] = fx match {
      case Left(value) => Left(value)
      case Right(value) => Right(func(value))
    }
  }
}
