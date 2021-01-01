package meow

trait Forall[F[_]] {
  def apply[A]: F[A]
}

trait ForallBounded[F[_]] {
  def apply[A : F]: A
}
