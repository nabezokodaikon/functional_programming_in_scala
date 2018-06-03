package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import language.higherKinds
import language.implicitConversions

// 主要な定義をParsersに配置する。
trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // run(or(string("abc"), string("def")))("abc") == Right("abc")
  // run(or(string("123"), string("456")))("456") == Right("456")
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  // run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char]

  // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  // run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  // run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  // Parsersの定義にParserOpsからデリゲートする。
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

  // List 9-2
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  // map(char('a'))(_.size)
  def many[A](p: Parser[A]): Parser[List[A]]

  // map(p)(a => a) == p
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  val numA: Parser[Int] = char('a').many.map(_.size)
}
