package meow

trait Semigroup[A] {
  def mplus(l: A, r: A): A
}
