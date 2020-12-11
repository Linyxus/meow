package meow

trait CurryFunctions {
  def curry2[A, B, C](func: (A, B) => C): A => B => C = x => y => func(x, y)
  def curry3[A, B, C, D](func: (A, B, C) => D): A => B => C => D = x => y => z => func(x, y, z)
}
