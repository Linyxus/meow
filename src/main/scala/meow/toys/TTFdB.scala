package meow.toys
import meow.ForallBounded

// Typed Tagless Final Interpreter with de Bruijn indices
trait TTFdB {
  // language symantics
  trait Symantics[Repr[_, _]] {
    // operations
    def int[Env](i: Int): Repr[Env, Int]
    def add[Env](l: Repr[Env, Int], r: Repr[Env, Int]): Repr[Env, Int]

    // de Bruijn indices
    def here[Env, V]: Repr[(V, Env), V]
    def before[A, Env, V](v: Repr[Env, V]): Repr[(A, Env), V]

    // lambda
    def lam[A, Env, B](v: Repr[(A, Env), B]): Repr[Env, A => B]
    def app[A, Env, B](func: Repr[Env, A => B], arg: Repr[Env, A]): Repr[Env, B]
  }

  // interpret by evaluating
  case class Eval[Env, A](eval: Env => A)
  implicit def evalSymantics: Symantics[Eval] = new Symantics[Eval] {
    override def int[Env](i: Int): Eval[Env, Int] = Eval(_ => i)

    override def add[Env](l: Eval[Env, Int], r: Eval[Env, Int]): Eval[Env, Int] = Eval { env =>
      l.eval(env) + r.eval(env)
    }

    override def here[Env, V]: Eval[(V, Env), V] = Eval { case (x, _) => x }

    override def before[A, Env, V](v: Eval[Env, V]): Eval[(A, Env), V] = Eval { case (_, rem) => v.eval(rem) }

    override def lam[A, Env, B](v: Eval[(A, Env), B]): Eval[Env, A => B] = Eval { env => a => v.eval((a, env)) }

    override def app[A, Env, B](func: Eval[Env, A => B], arg: Eval[Env, A]): Eval[Env, B] = Eval { env =>
      func.eval(env)(arg.eval(env))
    }
  }

  // eval the language
  def eval[Env, A](prog: Eval[Env, A], env: Env): A = prog.eval(env)

  // interpret by converting to string
  case class Show[Env, A](show: Int => String)
  implicit def showSymantics: Symantics[Show] = new Symantics[Show] {
    override def int[Env](i: Int): Show[Env, Int] = Show { _ => i.toString }

    override def add[Env](l: Show[Env, Int], r: Show[Env, Int]): Show[Env, Int] = Show { b =>
      s"(${l.show(b)} + ${r.show(b)})"
    }

    override def here[Env, V]: Show[(V, Env), V] = Show { b => s"x$b" }

    override def before[A, Env, V](v: Show[Env, V]): Show[(A, Env), V] = Show { x => v.show(x - 1) }

    override def lam[A, Env, B](v: Show[(A, Env), B]): Show[Env, A => B] = Show { b =>
      s"λ x$b . ${v.show(b + 1)}"
    }

    override def app[A, Env, B](func: Show[Env, A => B], arg: Show[Env, A]): Show[Env, B] = Show { b =>
      s"(${func.show(b)}) (${arg.show(b)})"
    }
  }

  // convert the expression to string
  def show[Env, A](expr: Show[Env, A]): String = expr.show(0)
}

object TTFdB extends TTFdB
