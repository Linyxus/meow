package meow

import meow.Peano._

sealed trait NList[N, +A]
case object NNil extends NList[Z, Nothing]
case class NCons[N, A](x: A, xs: NList[N, A]) extends NList[S[N], A]

object NList {
  def empty[A]: NList[Z, A] = NNil

  def headOf[N, A](xs: NList[S[N], A]): A = xs match {
    case NCons(x, _) => x
  }

  def tailOf[N, A](xs: NList[S[N], A]): NList[N, A] = xs match {
    case NCons(_, xs) => xs
  }
}
