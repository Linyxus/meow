package meow.monad

import meow._
import Meow._

case class WriterT[W : Monoid, F[_] : Monad, A](runWriterT: F[(A, W)]) {
  lazy val run: F[(A, W)] = runWriterT
  lazy val eval: F[A] = run <%| (_._1)
  lazy val exec: F[W] = run <%| (_._2)
}

trait WriterTFunctions extends WriterTInstances {
  def writer[W : Monoid, F[_] : Monad, A](aw: (A, W)): WriterT[W, F, A] = WriterT { aw.pure[F] }

  def tell[W : Monoid, F[_] : Monad](w: W): WriterT[W, F, Unit] = writer((), w)

  def listen[W : Monoid, F[_] : Monad, A](ma: WriterT[W, F, A]): WriterT[W, F, (A, W)] = WriterT {
    ma.runWriterT <%| { case (a, w) => ((a, w), w) }
  }

  def pass[W : Monoid, F[_] : Monad, A](ma: WriterT[W, F, (A, W => W)]): WriterT[W, F, A] = WriterT {
    ma.runWriterT <%| { case ((a, func), w) => (a, func(w)) }
  }
}

trait WriterTInstances {
  implicit def writerTIsMonad[W : Monoid, F[_] : Monad]: Monad[WriterT[W, F, *]] = new Monad[WriterT[W, F, *]] {
    override def andThen[A, B](ma: WriterT[W, F, A], fab: A => WriterT[W, F, B]): WriterT[W, F, B] = WriterT {
      ma.runWriterT >>= { case (a, w1) =>
        fab(a).runWriterT <%| { case (b, w2) => (b, w1 <> w2) }
      }
    }

    override def pureOf[A](x: A): WriterT[W, F, A] =
      WriterT { (x, implicitly[Monoid[W]].mzero).mreturn[F] }
  }
}

object WriterT extends WriterTFunctions with WriterTInstances
