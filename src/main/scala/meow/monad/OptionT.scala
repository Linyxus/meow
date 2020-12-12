package meow.monad

import meow._
import Meow._

case class OptionT[F[_], A](runOptionT: F[Option[A]])

trait OptionTFunctions {
  def fail[F[_], A](implicit applicative: Applicative[F], monad: Monad[F]): OptionT[F, A] =
    liftOption(None)

  def liftOption[F[_], A](mx: Option[A])(implicit applicative: Applicative[F], monad: Monad[F]): OptionT[F, A] =
    OptionT { mx.mreturn[F] }

  def lift[F[_], A](fx: F[A])(implicit functor: Functor[F], applicative: Applicative[F], monad: Monad[F]): OptionT[F, A] =
    OptionT { fx map (_.pure[Option]) }
}

trait OptionTInstances {
  implicit def optionTIsFunctor[F[_]: Functor]: Functor[OptionT[F, *]] =
    new Functor[OptionT[F, *]] {
      override def fmap[A, B](func: A => B, fx: OptionT[F, A]): OptionT[F, B] =
        OptionT(fx.runOptionT <%| (mx => mx <%| func))
    }

  implicit def optionTIsApplicative[F[_]: Applicative]: Applicative[OptionT[F, *]] =
    new Applicative[OptionT[F, *]] {
      override val functor: Functor[OptionT[F, *]] = optionTIsFunctor(implicitly[Applicative[F]].functor)

      override def pureOf[A](x: A): OptionT[F, A] = OptionT(x.pure[Option].pure[F])

      override def ap[A, B](mfunc: OptionT[F, A => B], ma: =>OptionT[F, A]): OptionT[F, B] =
        OptionT {
          val f: Option[A => B] => Option[A] => Option[B] = mab => ma => mab <*> ma
          f.pure[F] <*> mfunc.runOptionT <*> ma.runOptionT
        }
    }

  implicit def optionTIsMonad[F[_]: Monad](implicit applicative: Applicative[F]): Monad[OptionT[F, *]] =
    new Monad[OptionT[F, *]] {
      override val applicative: Applicative[OptionT[F, *]] = optionTIsApplicative(implicitly[Monad[F]].applicative)

      override def andThen[A, B](ma: OptionT[F, A], fab: A => OptionT[F, B]): OptionT[F, B] = {
        OptionT {
          ma.runOptionT >>= {
            case None => Option.empty[B].mreturn[F]
            case Some(value) => fab(value).runOptionT
          }
        }
      }
    }
}

object OptionT extends OptionTFunctions with OptionTInstances
