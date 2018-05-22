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
}
