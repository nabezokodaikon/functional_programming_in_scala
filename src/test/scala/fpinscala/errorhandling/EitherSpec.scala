package fpinscala.errorhandling

import org.scalatest.FunSuite

class EitherSpec extends FunSuite {

  test("List 4-7") {
    import Either.mean
    assert(mean(IndexedSeq[Double]()) == Left("mean of empty list!"))
    assert(mean(IndexedSeq(0.1)) == Right(0.1 / 1))
    assert(mean(IndexedSeq(0.1, 0.2)) == Right((0.1 + 0.2) / 2))
  }
}
