package meow

import meow.syntax.{AlternativeOpInstances, ApplicativePureOpInstances, ApplicativeOpInstances, CanFMapOpInstances}

trait Alternative[F[_]] {
  def or[A](fx: F[A], fy: F[A]): F[A]
  def mempty[A]: F[A]
}

trait AlternativeFunctions
  extends AlternativeInstances
    with AlternativeOpInstances
    with ApplicativePureOpInstances
    with ApplicativeOpInstances
    with CanFMapOpInstances {
  def mempty[F[_]: Alternative, A]: F[A] = implicitly[Alternative[F]].mempty[A]

  def some[F[_]: Alternative, A](v: F[A])(implicit applicative: Applicative[F], functor: Functor[F]): F[List[A]] = {
    val cons: A => List[A] => List[A] = x => xs => x :: xs
    cons <%> v <*> many(v)
  }

  def many[F[_]: Alternative, A](v: F[A])(implicit applicative: Applicative[F], functor: Functor[F]): F[List[A]] =
    some(v) <|> List.empty[A].pure[F]
}

trait AlternativeInstances {
  implicit val listIsAlternative: Alternative[List] = new Alternative[List] {
    override def or[A](fx: List[A], fy: List[A]): List[A] = fx ++ fy
    override def mempty[A]: List[A] = Nil
  }

  implicit val optionIsAlternative: Alternative[Option] = new Alternative[Option] {
    override def or[A](fx: Option[A], fy: Option[A]): Option[A] =
      if (fx.isDefined) fx else fy
    override def mempty[A]: Option[A] = None
  }
}
