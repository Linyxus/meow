package meow.syntax

import meow.Arrow

trait ArrowOp[F[_, _], A, B] {
  def first[C]: F[(A, C), (B, C)]
  def >>>[C](y: F[B, C]): F[A, C]
}

trait ArrowExtraOp[F[_, _], A, B] {
  def ***[C, D](y: F[C, D]): F[(A, C), (B, D)]
  def second[C]: F[(C, A), (C, B)]
  def &&&[C](y: F[A, C]): F[A, (B, C)]
}

trait ToArrowOp[A, B] {
  def arrow[F[_, _] : Arrow]: F[A, B]
}

trait ArrowOpInstances {
  implicit def toArrowOp[F[_, _] : Arrow, A, B](f: F[A, B]): ArrowOp[F, A, B] = new ArrowOp[F, A, B] {
    val arrow: Arrow[F] = implicitly

    override def first[C]: F[(A, C), (B, C)] = arrow.first(f)

    override def >>>[C](y: F[B, C]): F[A, C] = arrow.compose(f, y)
  }
}

trait ToArrowOpInstances {
  implicit def toToArrowOp[A, B](func: A => B): ToArrowOp[A, B] = new ToArrowOp[A, B] {
    override def arrow[F[_, _] : Arrow]: F[A, B] = implicitly[Arrow[F]].arr(func)
  }
}

trait ArrowExtraOpInstances extends ArrowOpInstances with ToArrowOpInstances {
  implicit def toArrowExtraOp[F[_, _] : Arrow, A, B](f: F[A, B]): ArrowExtraOp[F, A, B] = new ArrowExtraOp[F, A, B] {
    val arrow: Arrow[F] = implicitly

    def getSecond[M, N, C](g: F[M, N]): F[(C, M), (C, N)] = {
      def swap[X, Y]: F[(X, Y), (Y, X)] = arrow arr { case (x, y) => (y, x) }
      swap[C, M] >>> g.first[C] >>> swap[N, C]
    }

    override def second[C]: F[(C, A), (C, B)] = getSecond[A, B, C](f)

    override def ***[C, D](y: F[C, D]): F[(A, C), (B, D)] = f.first[C] >>> getSecond(y)

    override def &&&[C](y: F[A, C]): F[A, (B, C)] =
      { (x: A) => (x, x) }.arrow[F] >>> f.first[A] >>> getSecond(y)
  }
}
