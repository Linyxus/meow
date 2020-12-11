package meow.syntax

import meow.{CurryFunctions, Functor}

trait FunctorOp[F[_], A] {
  val functor: Functor[F]
  def <#>[B](func: A => B): F[B]
}

trait CanFMapOp[A, B] {
  def <%>[F[_]: Functor](fx: F[A]): F[B]
}

trait CanFMap2Op[A, B, C] {
  def <%%>[F[_]: Functor](fx: F[A]): F[B => C]
}

trait FunctorOpInstances {
  implicit def toFunctorOp[F[_]: Functor, A](fa: F[A]): FunctorOp[F, A] = new FunctorOp[F, A] {
    override val functor: Functor[F] = implicitly[Functor[F]]

    override def <#>[B](func: A => B): F[B] = functor.fmap(func, fa)
  }
}

trait CanFMapOpInstances extends FunctorOpInstances {
  implicit def toCanFMapOp[A, B](func: A => B): CanFMapOp[A, B] = new CanFMapOp[A, B] {
    override def <%>[F[_] : Functor](fx: F[A]): F[B] = fx <#> func
  }
}

trait CanFMap2OpInstances extends CurryFunctions with FunctorOpInstances {
  implicit def toCanFMap2Op[A, B, C](func: (A, B) => C): CanFMap2Op[A, B, C] = new CanFMap2Op[A, B, C] {
    override def <%%>[F[_] : Functor](fx: F[A]): F[B => C] = fx <#> curry2(func)
  }
}
