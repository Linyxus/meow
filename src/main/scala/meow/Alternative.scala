package meow

trait Alternative[F[_]] {
  def or[A](fx: F[A], fy: F[A]): F[A]
}

trait AlternativeInstances {
  implicit val listIsAlternative: Alternative[List] = new Alternative[List] {
    override def or[A](fx: List[A], fy: List[A]): List[A] = fx ++ fy
  }

  implicit val optionIsAlternative: Alternative[Option] = new Alternative[Option] {
    override def or[A](fx: Option[A], fy: Option[A]): Option[A] =
      if (fx.isDefined) fx else fy
  }

  implicit def eitherIsAlternative[E]: Alternative[({ type P[A] = Either[E, A] })#P] =
    new Alternative[({ type P[A] = Either[E, A] })#P] {
      override def or[A](fx: Either[E, A], fy: Either[E, A]): Either[E, A] =
        if (fx.isRight) fx else fy
    }
}
