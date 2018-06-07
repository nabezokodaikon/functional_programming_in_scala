package fpinscala.errorhandling

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Int =
    age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)

  /*
   * 最初に検出されたエラーを返すもの。
   */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h.map2(sequence_2(t))(_ :: _)
    }

  def sequence_2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

}

case class Person(name: Name, age: Age)
sealed case class Name(value: String)
sealed case class Age(value: Int)

object Person {

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}

/*
 * TODO
 * エラーを累積するデータ型。
 */
sealed trait Partial[+A, +B] {

  def map2[AA >: A, C, D](c: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] =
    (this, c) match {
      case (Success(x), Success(y)) => Success(f(x, y))
      case (Errors(x), Errors(y)) => Errors(x ++ y)
      case (Errors(x), _) => Errors(x)
      case (_, Errors(y)) => Errors(y)
    }
}

case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
case class Success[+B](get: B) extends Partial[Nothing, B]

object Person_2 {

  def mkName(name: String): Partial[String, Name] =
    if (name == "" || name == null) Errors(Seq("Name is empty."))
    else Success(Name(name))

  def mkAge(age: Int): Partial[String, Age] =
    if (age < 0) Errors(Seq("Age is out of range."))
    else Success(Age(age))

  def mkPerson(name: String, age: Int): Partial[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
