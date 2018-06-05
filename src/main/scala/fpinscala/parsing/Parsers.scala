package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import language.higherKinds
import language.implicitConversions

// 主要な定義をParsersに配置する。
trait Parsers[Parser[+_]] { self =>

  // Stringを1つ認識して返す。
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  /* EXERCISE 9.6
   * This code compile error.
  for {
    digit <- """(\d+)""".r
    val n: Int = digit.toInt
    _ <- listOfN(n, char('a'))
  } yield n
  */

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  // 常にaの値で成功する。
  def succeed[A](a: A): Parser[A]

  // 成功した場合はpが調べた部分の入力を返す。
  def slice[A](p: Parser[A]): Parser[String]

  // EXERCISE 9.5
  def wrap[A](p: => Parser[A]): Parser[A]

  // EXERCISE 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, wrap(many(p)))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  // EXERCISE 9.1 and EXERCISE 9.7
  // 2つのパーサーを逐次化してp1を実行したあとにp2を実行し、
  // 両方が成功した場合にそれらの結果をペアで返す。
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  // 成功した場合はpの結果に関数fを適用する。
  // EXERCISE 9.8
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))
  // flatMap(a)(f andThen succeed)

  // EXERCISE 9.1 and EXERCISE 9.7
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)
  /*
    EXERCISE 9.1
      map(product(p, p2))(f.tupled)
      map(product(p, p2))(ab => f(ab._1, ab._2))

    EXERCISE 9.7
      for {
        a <- p
        b <- p2
      } yield f(a, b)
   */

  // EXERCISE 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def attempt[A](p: Parser[A]): Parser[A]

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, b) => a)

  def whitespace: Parser[String] = "\\s*".r

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def thru(s: String): Parser[String] =
    (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] =
    string("\"") *> thru("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] =
    token(quoted label "string literal")

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  // Parsersの定義にParserOpsからデリゲートする。
  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] = self.flatMap(a)(f)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many = self.many(p)

    def slice[A](p: Parser[A]): Parser[String] = self.slice(p)

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // 2つのパーサーのどちらかを選択し、
  // 最初にp1を試した後、p1が失敗した場合にp2を試す。
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def orString(s1: String, s2: String): Parser[String]

  val numA: Parser[Int] = char('a').many.map(_.size)
}

// TODO: Location

// TODO: PaseError
