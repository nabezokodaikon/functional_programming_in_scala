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

  test("EXERCISE 10.2 optionMonoid") {
    import Monoid._

    assert(firstOptionMonoid[Int].op(Some(1), firstOptionMonoid[Int].op(Some(2), Some(3))) ==
      firstOptionMonoid[Int].op(firstOptionMonoid[Int].op(Some(1), Some(2)), Some(3)))

    assert(firstOptionMonoid[Int].op(None, firstOptionMonoid[Int].op(Some(2), Some(3))) == Some(2))
  }

  test("EXERCISE 10.3 endoMonoid") {
    import Monoid._

    val a = endoMonoid[Int].op(a => a + 2, endoMonoid[Int].op(b => b * 2, c => c / 2))
    val b = endoMonoid[Int].op(endoMonoid[Int].op(a => a + 2, b => b * 2), c => c / 2)
    assert(a(2) == b(2))
  }

  test("List 10-4 concatenate") {
    import Monoid._

    assert(concatenate(List(1, 2, 3), intAddition) == 6)
  }

  test("EXERCISE 10.5 foldMap") {
    import Monoid._

    assert(foldMap(List(1, 3, 5), booleanOr)(a => a % 2 == 0) == false)
    assert(foldMap(List(1, 2, 5), booleanOr)(a => a % 2 == 0) == true)
  }

  test("EXERCISE 10.6 foldRight") {
    import Monoid._

    assert(foldRight(List(1, 2, 3))(0)((a, b) => a + b) == 6)
  }

  test("EXERCISE 10.6 foldLeft") {
    import Monoid._

    assert(foldLeft(List(1, 2, 3))(0)((b, a) => a + b) == 6)
  }

  test("EXERCISE 10.7 foldMapV") {
    import Monoid._

    assert(foldMapV(IndexedSeq(1, 2, 3), (intAddition))(a => a * 2) == 12)
  }

  test("EXERCISE 10.11 count") {
    import Monoid._

    val s = "foo bar hoge"
    assert(count(s) == 3)
  }

  test("EXERCISE 10.12 ListFoldable") {
    import Monoid.intAddition
    import ListFoldable._

    assert(foldRight(List(1, 2, 3))(0)((a, b) => a + b) == 6)
    assert(foldLeft(List(1, 2, 3))(0)((b, a) => a + b) == 6)
    assert(foldMap(List(1, 2, 3))((a: Int) => a * 2)(intAddition) == 12)
    assert(concatenate(List(1, 2, 3))(intAddition) == 6)
  }
}
