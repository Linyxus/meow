package meow.monad

import meow._
import Meow._

case class Identity[+A](runIdentity: A)

trait IdentityInstances {
  implicit val identityIsMonad: Monad[Identity] = new Monad[Identity] {
    override def andThen[A, B](ma: Identity[A], fab: A => Identity[B]): Identity[B] =
      fab(ma.runIdentity)

    override def pureOf[A](x: A): Identity[A] = Identity(x)
  }
}

object Identity extends IdentityInstances
