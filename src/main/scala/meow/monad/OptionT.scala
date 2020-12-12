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
  implicit def optionTIsMonad[F[_]: Monad](implicit applicative: Applicative[F]): Monad[OptionT[F, *]] =
    new Monad[OptionT[F, *]] {
      override def andThen[A, B](ma: OptionT[F, A], fab: A => OptionT[F, B]): OptionT[F, B] = {
        OptionT {
          ma.runOptionT >>= {
            case None => Option.empty[B].mreturn[F]
            case Some(value) => fab(value).runOptionT
          }
        }
      }

      override def pureOf[A](x: A): OptionT[F, A] = OptionT(x.pure[Option].pure[F])
    }
}

object OptionT extends OptionTFunctions with OptionTInstances
