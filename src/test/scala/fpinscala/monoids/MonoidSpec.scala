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
}
