package meow

trait Functor[F[_]] {
  def fmap[A, B](func: A => B, fx: F[A]): F[B]
}
