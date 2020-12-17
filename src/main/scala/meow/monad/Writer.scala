package meow.monad

import meow._
import meow.monad.WriterT
import WriterT._
import meow.monad.Identity
import Identity._
import Isomorphisms._

case class Writer[W: Monoid, A](wrapped: WriterT[W, Identity, A]) {
  lazy val run: (A, W) = wrapped.run.runIdentity
  lazy val eval: A = run._1
  lazy val exec: W = run._2
}

trait WriterFunctions extends WriterInstances {
  def writer[W : Monoid, A](aw: (A, W)): Writer[W, A] = Writer(WriterT.writer(aw))

  def tell[W : Monoid](w: W): Writer[W, Unit] = writer((), w)

  def listen[W : Monoid, A](ma: Writer[W, A]): Writer[W, (A, W)] = Writer(WriterT.listen[W, Identity, A](ma.wrapped))

  def pass[W : Monoid, A](ma: Writer[W, (A, W => W)]): Writer[W, A] = Writer(WriterT.pass[W, Identity, A](ma.wrapped))
}

trait WriterInstances {
  def iso[W : Monoid]: IsoMonad[WriterT[W, Identity, *], Writer[W, *]] =
    new IsoMonad[WriterT[W, Identity, *], Writer[W, *]] {
      override def to[A]: WriterT[W, Identity, A] => Writer[W, A] = x => Writer(x)

      override def from[A]: Writer[W, A] => WriterT[W, Identity, A] = _.wrapped
    }

  implicit def writerIsMonad[W : Monoid]: Monad[Writer[W, *]] = iso[W].toInstance
}

object Writer extends WriterFunctions
