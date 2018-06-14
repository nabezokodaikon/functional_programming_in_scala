package fpinscala.monoids

import org.scalatest.FunSuite

class MonoidSpec extends FunSuite {

  test("List 10-1") {
    val m = new Monoid[Int] {
      def op(a1: Int, a2: Int) =
        a1 + a2

      def zero = 0
    }

    import m._
    assert(op(1, op(2, 3)) == op(op(1, 2), 3))
  }

  test("EXERCISE 10.1") {
    import Monoid._
    assert(intAddition.op(1, intAddition.op(2, 3)) == intAddition.op(intAddition.op(1, 2), 3))
    assert(intMultiplication.op(1, intMultiplication.op(2, 3)) == intMultiplication.op(intMultiplication.op(1, 2), 3))
    assert(booleanOr.op(true, booleanOr.op(false, false)) == booleanOr.op(booleanOr.op(true, false), false))
    assert(booleanAnd.op(true, booleanAnd.op(false, false)) == booleanAnd.op(booleanAnd.op(true, false), false))
  }
}
