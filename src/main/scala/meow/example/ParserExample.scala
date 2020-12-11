package meow.example

import meow._
import Meow._
import meow.monad._
import Parser._

object ParserExample extends App {
  val p1: Parser[Char] = char('1') >> char ('2') >> char('3')
  println(p1.runParser("1234"))
  println(p1.runParser("132"))

  val p2: Parser[List[Char]] = some(char('1'))
  println(p2.runParser("111"))

  val p3: Parser[String] = string("abc")
  println(p3.runParser("abc"))
  println(p3.parseOnly("abc"))
  println(p3.parseOnly("abcabc"))

  val p4: Parser[List[String]] = some(string("abc"))
  println(p4.parseOnly("abcabc"))

  val p5: Parser[List[String]] = string("abc") sepBy1 char(',')
  println(p5.parseOnly("abc,abc,abc"))

  // simple url parse
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
  println(pScheme parseOnly "http://")
  val pTerm: Parser[String] = some(satisfy(_.isLetter)) <%| (_ mkString "")
  println(pTerm parseOnly "abcd")
  val pUrl: Parser[Url] = Url <%%> pScheme <*> pTerm.sepBy1(char('.'))
  println(pUrl parseOnly "https://example.com.cn")
  println("https://example.com.cn" ~~> pUrl)
}
