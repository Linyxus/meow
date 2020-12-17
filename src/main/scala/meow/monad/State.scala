package meow.monad

import meow._
import Isomorphisms._

case class State[S, A](wrapped: StateT[S, Identity, A]) {
  def run(s: S): (A, S) = wrapped.run(s).runIdentity
  def eval(s: S): A = run(s)._1
  def exec(s: S): S = run(s)._2
}

trait StateFunctions extends StateInstances {
  def get[S]: State[S, S] = iso[S].to(StateT.get[S, Identity])
  def modify[S](func: S => S): State[S, Unit] = iso[S].to(StateT.modify[S, Identity](func))
  def set[S](x: S): State[S, Unit] = iso[S].to(StateT.set[S, Identity](x))
}

trait StateInstances {
  def iso[S]: IsoMonad[StateT[S, Identity, *], State[S, *]] = new IsoMonad[StateT[S, Identity, *], State[S, *]] {
    override def from[A]: State[S, A] => StateT[S, Identity, A] = st => st.wrapped

    override def to[A]: StateT[S, Identity, A] => State[S, A] = st => State(st)
  }

  implicit def stateIsMonad[S]: Monad[State[S, *]] = iso[S].toInstance
}

object State extends StateInstances with StateFunctions

