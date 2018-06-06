package fpinscala.monoids

import org.scalatest.FunSuite

class MonoidSpec extends FunSuite {

  test("List 10-2 stringMonoid") {
    import Monoid.stringMonoid.op
    val a = op(op("a", "b"), "c")
    val b = op("a", op("b", "c"))
    assert(a == b)
  }

  test("List 10-3 listMonoid") {
    import Monoid.listMonoid
    val a = listMonoid.op(listMonoid.op(List(1, 2), List(3, 4)), List(5, 6))
    val b = listMonoid.op((List(1, 2)), listMonoid.op(List(3, 4), List(5, 6)))
    assert(a == b)
  }

  test("EXERCISE 10.1 intAddition") {
    import Monoid.intAddition.op
    val a = op(op(1, 2), 3)
    val b = op(1, op(2, 3))
    assert(a == b)
  }

  test("EXERCISE 10.1 intMultiplication") {
    import Monoid.intMultiplication.op
    val a = op(op(1, 2), 3)
    val b = op(1, op(2, 3))
    assert(a == b)
  }

  test("EXERCISE 10.1 booleanOr") {
    import Monoid.booleanOr.op
    val a = op(op(true, true), true)
    val b = op(true, op(true, true))
    val c = op(op(false, false), false)
    val d = op(false, op(false, false))
    val e = op(op(false, true), false)
    val f = op(false, op(true, false))
    val g = op(op(false, true), true)
    val h = op(true, op(true, false))
    assert(a == b == e == f == g == h)
    assert(c == d)
    assert(a != c)
  }

  test("EXERCISE 10.1 booleanAnd") {
    import Monoid.booleanOr.op
    val a = op(op(true, true), true)
    val b = op(true, op(true, true))
    val c = op(op(false, false), false)
    val d = op(false, op(false, false))
    val e = op(op(false, true), false)
    val f = op(false, op(true, false))
    val g = op(op(false, true), true)
    val h = op(true, op(true, false))
    assert(a == b)
    assert(c == d == e == f == g == h)
    assert(a != c)
  }

  test("EXERCISE 10.2 firstOptionMonoid") {
    import Monoid.firstOptionMonoid
    val a = firstOptionMonoid.op(firstOptionMonoid.op(Some(1), Some(2)), Some(3))
    assert(a.isInstanceOf[Some[Int]] == true)
    val b = firstOptionMonoid.op(firstOptionMonoid.op(Some(1), None), Some(3))
    assert(b.isInstanceOf[Some[Int]] == true)
    val c = firstOptionMonoid.op(firstOptionMonoid.op(Some(1), None), None)
    assert(c.isInstanceOf[Some[Int]] == true)
    val d = firstOptionMonoid.op(firstOptionMonoid.op(None, None), None)
    assert(d == None)
  }

  test("exercise 10.2 lastOptionMonoid") {
    import Monoid.lastOptionMonoid
    val a = lastOptionMonoid.op(lastOptionMonoid.op(Some(1), Some(2)), Some(3))
    assert(a.isInstanceOf[Some[Int]] == true)
    val b = lastOptionMonoid.op(lastOptionMonoid.op(Some(1), None), Some(3))
    assert(b.isInstanceOf[Some[Int]] == true)
    val c = lastOptionMonoid.op(lastOptionMonoid.op(Some(1), None), None)
    assert(c.isInstanceOf[Some[Int]] == true)
    val d = lastOptionMonoid.op(lastOptionMonoid.op(None, None), None)
    assert(d == None)
  }

  test("EXERCISE 10.3 endMonoid") {
    import Monoid.endMonoid
    val a = endMonoid.op(endMonoid.op(((x: Int) => x + 1), ((x: Int) => x + 2)), ((x: Int) => x + 3))
    assert(a(2) == 8)
  }
}
