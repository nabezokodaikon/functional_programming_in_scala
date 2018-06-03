package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import language.higherKinds
import language.implicitConversions

// 主要な定義をParsersに配置する。
trait Parsers[ParseError, Parser[+_]] { self =>

  // run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)

  // Parsersの定義にParserOpsからデリゲートする。
  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many = self.many(p)

    def slice[A](p: Parser[A]): Parser[String] = self.slice(p)

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def orString(s1: String, s2: String): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  val numA: Parser[Int] = char('a').many.map(_.size)
}
