package meow

trait Arrow[F[_, _]] {
  def arr[A, B](func: A => B): F[A, B]
  def first[A, B, C](a: F[A, B]): F[(A, C), (B, C)]
  def compose[A, B, C](l: F[A, B], r: F[B, C]): F[A, C]
}

trait ArrowInstances {
  implicit def functionIsArrow: Arrow[* => *] = new Arrow[Function] {
    override def arr[A, B](func: A => B): Function[A, B] = identity(func)

    override def first[A, B, C](arrow: Function[A, B]): Function[(A, C), (B, C)] = { case (a, c) => (arrow(a), c) }

    override def compose[A, B, C](l: Function[A, B], r: Function[B, C]): Function[A, C] = x => r(l(x))
  }
}
