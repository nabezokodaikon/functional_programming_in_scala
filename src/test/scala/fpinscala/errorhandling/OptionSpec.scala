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
}
