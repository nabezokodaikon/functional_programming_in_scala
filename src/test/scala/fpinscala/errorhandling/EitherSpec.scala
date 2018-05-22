package fpinscala.errorhandling

import org.scalatest.FunSuite

class EitherSpec extends FunSuite {

  test("List 4-7") {
    import Either.mean
    assert(mean(IndexedSeq[Double]()) == Left("mean of empty list!"))
    assert(mean(IndexedSeq(0.1)) == Right(0.1 / 1))
    assert(mean(IndexedSeq(0.1, 0.2)) == Right((0.1 + 0.2) / 2))
  }

  test("List 4-8") {
    import Either.safeDiv
    assert(safeDiv(1, 0).isInstanceOf[Left[Exception]])
    assert(safeDiv(4, 2) == Right(2))
  }

  test("List 4-9") {
    import Either.Try
    assert(Try(1 / 0).isInstanceOf[Left[Exception]])
    assert(Try(2 / 2) == Right(1))
  }

  test("EXERCISE 4.6 map") {
    assert((Left("a"): Either[String, Int]).map(_ * 2) == Left("a"))
    assert(Right(2).map(_ * 2) == Right(4))
  }

  test("EXERCISE 4.6 flatMap") {
    assert((Left("a"): Either[String, Int]).flatMap(a => Right(a * 2)) == Left("a"))
    assert(Right(2).flatMap(a => Right(a * 2)) == Right(4))
  }

  test("EXERCISE 4.6 orElse") {
    assert((Left("a"): Either[String, Int]).orElse(Left("b")) == Left("b"))
    assert(Right(2).orElse(Left("2")) == Right(2))
  }

  test("EXERCISE 4.6 map2") {
    val la = Left("a"): Either[String, Int]
    val lb = Left("b"): Either[String, Int]

    assert(la.map2(lb)((a, b) => a * b) == Left("a"))
    assert(la.map2(Right(3))((a, b) => a * b) == Left("a"))
    assert(Right(2).map2(lb)((a, b) => a * b) == Left("b"))
    assert(Right(2).map2(Right(3))((a, b) => a * b) == Right(6))
  }

  test("parseInsuranceRateQuote") {
    import Either.parseInsuranceRateQuote
    assert(parseInsuranceRateQuote("a", "b").isInstanceOf[Left[Exception]])
    assert(parseInsuranceRateQuote("2", "b").isInstanceOf[Left[Exception]])
    assert(parseInsuranceRateQuote("a", "3").isInstanceOf[Left[Exception]])
    assert(parseInsuranceRateQuote("2", "3") == Right(6))
  }

  test("EXERCISE 4.7 sequence") {
    import Either.sequence
    assert(sequence(List[Either[String, Int]]()) == Right(Nil))
    assert(sequence(List(Left("a"))) == Left("a"))
    assert(sequence(List(Left("a"), Left("b"))) == Left("a"))
    assert(sequence(List(Right(1), Left("a"))) == Left("a"))
    assert(sequence(List(Left("a"), Right(1))) == Left("a"))
    assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
  }

  test("EXERCISE 4.7 sequence_2") {
    import Either.sequence_2
    assert(sequence_2(List[Either[String, Int]]()) == Right(Nil))
    assert(sequence_2(List(Left("a"))) == Left("a"))
    assert(sequence_2(List(Left("a"), Left("b"))) == Left("a"))
    assert(sequence_2(List(Right(1), Left("a"))) == Left("a"))
    assert(sequence_2(List(Left("a"), Right(1))) == Left("a"))
    assert(sequence_2(List(Right(1), Right(2))) == Right(List(1, 2)))
  }

  test("List 4-10 Person") {
    import Person._

    assert(mkName("") == Left("Name is empty."))
    assert(mkName(null) == Left("Name is empty."))
    assert(mkName("taro") == Right(Name("taro")))

    assert(mkAge(-1) == Left("Age is out of range."))
    assert(mkAge(0) == Right(Age(0)))
    assert(mkAge(1) == Right(Age(1)))

    assert(mkPerson("", 1) == Left("Name is empty."))
    assert(mkPerson("taro", -1) == Left("Age is out of range."))
    assert(mkPerson("", -1) == Left("Name is empty."))
    assert(mkPerson("taro", 1) == Right(Person(Name("taro"), Age(1))))
  }

  test("EXERCISE 4.8") {
    import Person_2._
    assert(mkPerson("", 1) == Errors(Seq("Name is empty.")))
    assert(mkPerson("taro", -1) == Errors(Seq("Age is out of range.")))
    assert(mkPerson("", -1) == Errors(Seq("Name is empty.", "Age is out of range.")))
    assert(mkPerson("taro", 1) == Success(Person(Name("taro"), Age(1))))
  }
}
