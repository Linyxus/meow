package meow

trait Isomorphisms {
  trait IsoSet[A, B] {
    def to: A => B
    def from: B => A
  }

  type <=>[A, B] = IsoSet[A, B]
}

object Isomorphisms extends Isomorphisms
