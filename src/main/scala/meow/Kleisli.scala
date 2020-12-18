package meow

case class Kleisli[M[_] : Monad, A, B](runKleisli: A => M[B])

trait KleisliInstances {
  implicit def kleisliIsArrow[M[_] : Monad]: Arrow[Kleisli[M, *, *]] = new Arrow[Kleisli[M, *, *]] {
    val monad: Monad[M] = implicitly

    override def arr[A, B](func: A => B): Kleisli[M, A, B] = Kleisli { a =>
      monad.pureOf(func(a))
    }

    override def first[A, B, C](fab: Kleisli[M, A, B]): Kleisli[M, (A, C), (B, C)] = Kleisli { case (a, c) =>
      monad.andThen[B, (B, C)](fab.runKleisli(a), { b => monad.pureOf(b, c) })
    }

    override def compose[A, B, C](l: Kleisli[M, A, B], r: Kleisli[M, B, C]): Kleisli[M, A, C] = Kleisli { a =>
      monad.andThen(l.runKleisli(a), r.runKleisli)
    }
  }
}
