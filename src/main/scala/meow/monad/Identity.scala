package meow.monad

import meow._
import Meow._

case class Identity[+A](runIdentity: A)

trait IdentityInstances {
  implicit def identityIsFunctor: Functor[Identity] = new Functor[Identity] {
    override def fmap[A, B](func: A => B, fx: Identity[A]): Identity[B] = Identity { func(fx.runIdentity) }
  }

  implicit val identityIsApplicative: Applicative[Identity] = new Applicative[Identity] {
    override val functor: Functor[Identity] = implicitly

    override def pureOf[A](x: A): Identity[A] = Identity(x)

    override def ap[A, B](mfunc: Identity[A => B], ma: =>Identity[A]): Identity[B] =
      Identity { mfunc.runIdentity(ma.runIdentity) }
  }

  implicit val identityIsMonad: Monad[Identity] = new Monad[Identity] {
    override val applicative: Applicative[Identity] = implicitly

    override def andThen[A, B](ma: Identity[A], fab: A => Identity[B]): Identity[B] =
      (fab <%> ma).runIdentity
  }
}

object Identity extends IdentityInstances
