package meow

trait Monoid[A] extends Semigroup[A] {
  def mzero: A
}

trait MonoidInstances {
  implicit def listIsMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def mzero: List[A] = List.empty[A]

    override def mplus(l: List[A], r: List[A]): List[A] = l ++ r
  }
}
