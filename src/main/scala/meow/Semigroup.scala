package meow

trait Semigroup[A] {
  def mplus(l: A, r: A): A
}

trait SemigroupInstances {
  implicit def listIsSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def mplus(l: List[A], r: List[A]): List[A] = l ++ r
  }
}
