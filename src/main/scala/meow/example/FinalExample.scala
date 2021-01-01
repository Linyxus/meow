package meow.example

import meow.ForallBounded

trait ExpSymb[A] {
  def lit(i: Int): A
  def neg(a: A): A
  def add(a: A, b: A): A
  def mul(a: A, b: A): A
}

object FinalExample extends App {
  implicit val expCanEval: ExpSymb[Int] = new ExpSymb[Int] {
    override def lit(i: Int): Int = i

    override def neg(a: Int): Int = -a

    override def add(a: Int, b: Int): Int = a + b

    override def mul(a: Int, b: Int): Int = a * b
  }

  def eval(e: ForallBounded[ExpSymb]): Int = e.apply[Int]

  implicit val expCanView: ExpSymb[String] = new ExpSymb[String] {
    override def lit(i: Int): String = i.toString

    override def neg(a: String): String = s"(- $a)"

    override def add(a: String, b: String): String = s"(+ $a $b)"

    override def mul(a: String, b: String): String = s"(* $a $b)"
  }

  def view(e: ForallBounded[ExpSymb]): String = e.apply[String]

  val prog1: ForallBounded[ExpSymb] = new ForallBounded[ExpSymb] {
    override def apply[A: ExpSymb]: A = {
      val f: ExpSymb[A] = implicitly
      f.add(f.lit(3), f.neg(f.lit(1)))
    }
  }

  type ExpProg = ForallBounded[ExpSymb]

  def lit(i: Int): ExpProg = new ExpProg {
    override def apply[A: ExpSymb]: A = implicitly[ExpSymb[A]].lit(i)
  }

  def neg(e: ExpProg): ExpProg = new ExpProg {
    override def apply[A: ExpSymb]: A = implicitly[ExpSymb[A]].neg(e.apply[A])
  }

  def add(e1: ExpProg, e2: ExpProg): ExpProg = new ExpProg {
    override def apply[A: ExpSymb]: A = implicitly[ExpSymb[A]].add(e1.apply[A], e2.apply[A])
  }

  def mul(e1: ExpProg, e2: ExpProg): ExpProg = new ExpProg {
    override def apply[A: ExpSymb]: A = implicitly[ExpSymb[A]].mul(e1.apply[A], e2.apply[A])
  }

  val prog2: ExpProg = neg(add(lit(1), mul(lit(2), lit(3))))

  println(view(prog1))
  println(eval(prog1))

  println(view(prog2))
  println(eval(prog2))
}
