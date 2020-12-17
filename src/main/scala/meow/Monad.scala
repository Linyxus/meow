package meow

trait Monad[F[_]] extends Applicative[F] {
  val applicative: Applicative[F] = this

  def returnOf[A](x: A): F[A] = applicative.pureOf(x)

  def andThen[A, B](ma: F[A], fab: A => F[B]): F[B]

  override def ap[A, B](mfunc: F[A => B], ma: => F[A]): F[B] =
    andThen[A => B, B](mfunc, { func => andThen[A, B](ma, { a => returnOf[B](func(a)) }) })
}

trait MonadInstances {
  implicit val listIsMonad: Monad[List] = new Monad[List] {
    override def andThen[A, B](ma: List[A], fab: A => List[B]): List[B] = ma flatMap fab
    override def pureOf[A](x: A): List[A] = List(x)
  }

  implicit val optionIsMonad: Monad[Option] = new Monad[Option] {
    override def andThen[A, B](ma: Option[A], fab: A => Option[B]): Option[B] = ma flatMap fab

    override def pureOf[A](x: A): Option[A] = Option(x)
  }

  implicit def eitherIsMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def andThen[A, B](ma: Either[E, A], fab: A => Either[E, B]): Either[E, B] = ma flatMap fab

    override def pureOf[A](x: A): Either[E, A] = Right(x)
  }

  implicit def functionIsMonad[C]: Monad[C => *] = new Monad[Function[C, *]] {
    override def andThen[A, B](ma: Function[C, A], fab: A => Function[C, B]): Function[C, B] =
      c => fab(ma(c))(c)

    override def pureOf[A](x: A): Function[C, A] = _ => x
  }
}
