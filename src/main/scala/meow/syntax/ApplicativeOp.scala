package meow.syntax

import meow.Applicative

trait ApplicativeOp[F[_], A, B] {
  val applicative: Applicative[F]

  def <*>(mval: F[A]): F[B]
}

trait ApplicativePureOp[A] {
  def pure[F[_]: Applicative]: F[A]
}

trait ApplicativeOpInstances {
  implicit def toApplicativeOp[F[_]: Applicative, A, B](mfunc: F[A => B]): ApplicativeOp[F, A, B] = new ApplicativeOp[F, A, B] {
    override val applicative: Applicative[F] = implicitly[Applicative[F]]

    override def <*>(mval: F[A]): F[B] = applicative.ap(mfunc, mval)
  }
}

trait ApplicativePureOpInstances {
  implicit def toApplicativePureOp[A](x: A): ApplicativePureOp[A] = new ApplicativePureOp[A] {
    override def pure[F[_] : Applicative]: F[A] = {
      val applicative: Applicative[F] = implicitly[Applicative[F]]
      applicative.pureOf(x)
    }
  }
}
