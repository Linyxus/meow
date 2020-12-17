package meow

import meow.syntax.IsoMonadOpInstances

trait Isomorphisms {
  trait IsoSet[A, B] {
    def to: A => B
    def from: B => A
  }

  type <=>[A, B] = IsoSet[A, B]

  trait IsoMonad[F[_], G[_]] {
    def to[A]: F[A] => G[A]
    def from[A]: G[A] => F[A]
  }
}

object Isomorphisms extends Isomorphisms with IsoMonadOpInstances
