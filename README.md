# meow
 A cute little library for Cats.

## Importing

```scala
import meow._
import Meow._
```

## Functor

See [Examples](src/main/scala/meow/example/FunctorExample.scala).

```scala
val m1: Option[Int] = Some(1)
m1 <#> (_ + 1)
// => Some(2)

val m2: List[Int] = (0 until 3).toList
m2 <#> (_ + 1)
// => List(1, 2, 3)
```

Do it in Haskell style:
```scala
val m1: Option[Int] = Some(1)
((x: Int) => x + 1) <%> m1
// => Some(2)

val m2: List[Int] = (0 until 3).toList
((x: Int) => x + 1) <%> m2
// => List(1, 2, 3)
```

> Use `<%>` instead of `<$>`.

## Applicative

See [Examples](src/main/scala/meow/example/ApplicativeExample.scala).

## Monad

See [Examples](src/main/scala/meow/example/MonadExample.scala).

## Currying

Currying a function:
```scala
def add(x: Int, y: Int): Int = x + y
val f: Int => Int => Int = curry2(add)
```

Currying a 2-ary function implicitly when mapping:
```scala
def add(x: Int, y: Int): Int = x + y
add _ <%%> List(1, 2) <*> List(3, 4)
```
