package meow.monad.syntax

import meow._
import Meow._
import NaturalTransformation._
import meow.monad.{Free, Join, Pure}

trait FreeOp[F[_], A] {
  def monad(implicit monad: Monad[F]): F[A]

  def transform[G[_] : Functor](f: F ~> G): Free[G, A]

  def transformMonad[G[_] : Monad](f: F ~> G): G[A]
}

trait FreeLiftOp[F[_], A] {
  def liftFree: Free[F, A]
}

trait FreeOpInstances {
  implicit def toFreeOp[F[_]: Functor, A](mx: Free[F, A]): FreeOp[F, A] = new FreeOp[F, A] {
    override def monad(implicit monad: Monad[F]): F[A] = {
      def go(fx: Free[F, A]): F[A] = fx match {
        case Join(ffa) =>
          val x: F[Free[F, A]] = ffa
          x >>= go
        case Pure(x) => x.mreturn[F]
      }
      go(mx)
    }

    override def transform[G[_] : Functor](f: F ~> G): Free[G, A] = {
      def go(fx: Free[F, A]): Free[G, A] = fx match {
        case Join(ffa) =>
          val x: F[Free[F, A]] = ffa
          Join(f(x <%| go))
        case Pure(x) => Pure(x)
      }
      go(mx)
    }

    override def transformMonad[G[_] : Monad](f: F ~> G): G[A] =
      toFreeOp(this.transform(f)).monad
  }
}

trait FreeLiftOpInstances {
  implicit def toFreeLiftOp[F[_]: Functor, A](mx: F[A]): FreeLiftOp[F, A] = new FreeLiftOp[F, A] {
    override def liftFree: Free[F, A] =
      Join(implicitly[Functor[F]].fmap[A, Free[F, A]]({ x => Pure(x) }, mx))
  }
}

