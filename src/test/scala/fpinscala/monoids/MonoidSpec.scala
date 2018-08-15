package fpinscala.monoids

import org.scalatest.FunSuite

class MonoidSpec extends FunSuite {
  import fpinscala.monoids._

  test("EXERCISE 10.1 intAddition") {
    import Monoid.intAddition._

    assert(op(op(1, 2), 3) == op(1, op(2, 3)))
    assert(op(zero, 1) == 1)
  }

  test("EXERCISE 10.1 intMultiplication") {
    import Monoid.intMultiplication._

    assert(op(op(1, 2), 3) == op(1, op(2, 3)))
    assert(op(zero, 1) == 1)
  }

  test("EXERCISE 10.1 booleanOr") {
    import Monoid.booleanOr._

    assert(op(op(true, false), true) == op(true, op(false, true)))
    assert(op(zero, true) == true)
    assert(op(zero, false) == false)
  }

  test("EXERCISE 10.1 booleanAnd") {
    import Monoid.booleanAnd._

    assert(op(op(true, false), true) == op(true, op(false, true)))
    assert(op(zero, true) == true)
    assert(op(zero, false) == false)
  }

  test("EXERCISE 10.2 optionMonoid") {
    import Monoid._

    val m = optionMonoid[Int]

    assert(m.op(m.op(Some(1), None), Some(2)) == m.op(Some(1), m.op(None, Some(2))))
    assert(m.op(m.zero, Some(1)) == Some(1))
    assert(m.op(m.zero, None) == None)
  }

  test("EXERCISE 10.3 endoMonoid") {
    import Monoid._
    // TODO
  }

  test("List 10-4 concatenate") {
    import Monoid._

    assert(concatenate(List(1, 2, 3), intAddition) == 6)
  }
}
