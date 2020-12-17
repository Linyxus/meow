package meow.syntax
import meow.{Monad, Isomorphisms}
import Isomorphisms._

trait IsoMonadOp[F[_], G[_]] {
  def toInstance: Monad[G]
}

trait IsoMonadOpInstances {
  implicit def toIsoMonadOp[F[_] : Monad, G[_]](isoMonad: IsoMonad[F, G]): IsoMonadOp[F, G] = new IsoMonadOp[F, G] {
    override def toInstance: Monad[G] = new Monad[G] {
        val monad: Monad[F] = implicitly

        override def andThen[A, B](ma: G[A], fab: A => G[B]): G[B] = {
          def gab(x: A): F[B] = isoMonad.from[B](fab(x))
          isoMonad.to(monad.andThen(isoMonad.from[A](ma), gab))
        }

        override def pureOf[A](x: A): G[A] = isoMonad.to[A](monad.pureOf(x))
      }
  }
}
