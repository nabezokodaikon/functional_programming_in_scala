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

  test("List 4-4") {
    import Option.mean_2

    assert(mean_2(Seq[Double]()) == None)
    assert(mean_2(Seq(0.1, 0.2)) == Some((0.1 + 0.2) / 2))
  }

  test("EXERCISE 4.1 map") {
    assert(None.map(a => a) == None)
    assert(Some(2).map(a => a * 2) == Some(4))
  }

  test("EXERCISE 4.1 getOrElse") {
    assert(None.getOrElse(0) == 0)
    assert(Some(2).getOrElse(0) == 2)
  }

  test("EXERCISE 4.1 flatMap") {
    assert(None.flatMap(a => a) == None)
    assert(Some(2).flatMap(a => Some(a * 2)) == Some(4))
  }

  test("EXERCISE 4.1 orElse") {
    assert(None.orElse(None) == None)
    assert(None.orElse(Some(1)) == Some(1))
    assert(Some(2).orElse(Some(1)) == Some(2))
  }

  test("EXERCISE 4.1 filter") {
    assert(None.filter(a => a == 1) == None)
    assert(Some(2).filter(a => a == 1) == None)
    assert(Some(2).filter(a => a == 2) == Some(2))
  }

  test("EXERCISE 4.2 variance") {
    import Option.variance
    assert(variance(Seq[Double]()) == None)
    assert(variance(Seq(0)) == Some(0.0))
    assert(variance(Seq(71, 80, 89)) == Some(54.0))
  }

  test("lift") {
    import Option.absO
    assert(absO(Some(-1)) == Some(1))
  }

  test("EXERCISE 4.3 map2") {
    import Option.map2
    assert(map2(None, None)((a: Int, b: Int) => a + b) == None)
    assert(map2(Some(1), None)((a, b) => a + b) == None)
    assert(map2(None, Some(2))((a: Int, b: Int) => a + b) == None)
    assert(map2(Some(1), Some(2))((a, b) => a + b) == Some(3))
  }

  test("insuranceRateQuote") {
    import Option.parseInsuranceRateQuote
    assert(parseInsuranceRateQuote("a", "b") == None)
    assert(parseInsuranceRateQuote("1", "b") == None)
    assert(parseInsuranceRateQuote("a", "2") == None)
    assert(parseInsuranceRateQuote("1", "2") == Some(3.0))
  }
}
