package meow

trait Applicative[F[_]] extends Functor[F] {
  val functor: Functor[F] = this
  def pureOf[A](x: A): F[A]
  def ap[A, B](mfunc: F[A => B], ma: => F[A]): F[B]

  override def fmap[A, B](func: A => B, fx: F[A]): F[B] = ap(pureOf(func), fx)
}
