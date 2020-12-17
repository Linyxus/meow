package meow

trait Isomorphisms {
  trait IsoSet[A, B] {
    def from: A => B
    def to: A => B
  }

  type <=>[A, B] = IsoSet[A, B]
}

object Isomorphisms extends Isomorphisms
