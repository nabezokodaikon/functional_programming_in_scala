package fpinscala.errorhandling

import org.scalatest.FunSuite

class OptionSpec extends FunSuite {

  test("List 4-1") {
    import Option.failingFn

    val caught = intercept[Exception] {
      failingFn(2)
    }
    assert(caught.getMessage == "fail!")

    val caught2 = intercept[Exception] {
      failingFn(3)
    }
    assert(caught2.getMessage == "fail!")
  }

  test("List 4-2") {
    import Option.failingFn2

    assert(failingFn2(2) == 43)
    assert(failingFn2(3) == 43)
  }

  test("mean") {
    import Option.mean

    val caught = intercept[Exception] {
      mean(Nil)
    }
    assert(caught.getMessage == "mean of empty list!")

    assert(mean(List(0.1, 0.2)) == (0.1 + 0.2) / 2)
  }

  test("mean_1") {
    import Option.mean_1

    assert(mean_1(IndexedSeq[Double](), 0.0) == 0.0)
    assert(mean_1(IndexedSeq(0.1, 0.2), 0.0) == (0.1 + 0.2) / 2)
  }
}
