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

## State Monad

See [Examples](src/main/scala/meow/example/StateExample.scala).

```scala
val mult: (Int, Int) => Int = (x, y) => x * y
val app: State[Int, Unit] =
  mult <%%> get[Int] <*> get >>= set
app.exec(-5)
```

## Parser Monad (Parser Combinator)

See [Examples](src/main/scala/meow/example/ParserExample.scala).

```scala
  sealed trait UrlScheme
case object Http extends UrlScheme
case object Https extends UrlScheme
case object Ftp extends UrlScheme
case object File extends UrlScheme

case class Url(scheme: UrlScheme, term: List[String])
val pScheme: Parser[UrlScheme] = choice(
  string("https") <*| Https,
  string("http") <*| Http,
  string("ftp") <*| Ftp,
  string("file") <*| File,
) <* string("://")
val pTerm: Parser[String] = some(satisfy(_.isLetter)) <%| (_ mkString "")
val pUrl: Parser[Url] = Url <%%> pScheme <*> pTerm.sepBy1(char('.'))
println(pUrl parseOnly "https://example.com.cn")
// => Some(Url(Https,List(example, com, cn)))
```
