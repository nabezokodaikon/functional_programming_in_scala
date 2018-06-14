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
}
