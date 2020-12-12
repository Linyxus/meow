package meow.monad

import meow._
import Meow._

case class ReaderT[E, F[_], A](runReaderT: E => F[A])

trait ReaderTFunctions {
  def read[E, F[_]: Monad]: ReaderT[E, F, E] = ReaderT { e => e.pure[F] }
}

trait ReaderTInstances {
  implicit def readerTIsMonad[E, F[_]: Monad]: Monad[ReaderT[E, F, *]] = new Monad[ReaderT[E, F, *]] {
    override def andThen[A, B](ma: ReaderT[E, F, A], fab: A => ReaderT[E, F, B]): ReaderT[E, F, B] = ReaderT { env =>
      ma.runReaderT(env) >>= { a => fab(a).runReaderT(env) }
    }

    override def pureOf[A](x: A): ReaderT[E, F, A] = ReaderT { _ => x.pure[F] }
  }
}
