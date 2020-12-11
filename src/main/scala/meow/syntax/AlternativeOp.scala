package meow.syntax

import meow.Alternative

trait AlternativeOp[F[_], A] {
  val alternative: Alternative[F]
  def <|>(fy: F[A]): F[A]
}

trait AlternativeOpInstances {
  implicit def toAlternativeOp[F[_]: Alternative, A](fx: F[A]): AlternativeOp[F, A] =
    new AlternativeOp[F, A] {
      override val alternative: Alternative[F] = implicitly
      override def <|>(fy: F[A]): F[A] = alternative.or(fx, fy)
    }
}
