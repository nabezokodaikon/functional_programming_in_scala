package fpinscala.datastructures

import org.scalatest.FunSuite

class ListSpec extends FunSuite {
  test("3-2 tail") {
    assertThrows[RuntimeException] {
      List.tail(Nil)
    }
    assert(List.tail(List(1)) == Nil)
    assert(List.tail(List(1, 2)) == List(2))
  }
}
