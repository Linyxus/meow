package meow.syntax

import meow.Semigroup

trait SemigroupOp[A] {
  def <>(y: A): A
}

trait SemigroupOpInstances {
  implicit def toSemigroupOp[A: Semigroup](x: A): SemigroupOp[A] = new SemigroupOp[A] {
    override def <>(y: A): A = implicitly[Semigroup[A]].mplus(x, y)
  }
}
