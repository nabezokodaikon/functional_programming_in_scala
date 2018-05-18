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

  test("EXERCISE 3.3 setHead") {
    assertThrows[RuntimeException] {
      List.setHead(1, Nil)
    }

    assert(List.setHead(9, List(1)) == List(9))
    assert(List.setHead(9, List(1, 2)) == List(9, 2))
  }

  test("EXERCISE 3.4 drop") {
    assert(List.drop(Nil, 1) == Nil)
    assert(List.drop(List(1, 2, 3, 4), -1) == List(1, 2, 3, 4))
    assert(List.drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
    assert(List.drop(List(1, 2, 3, 4), 1) == List(2, 3, 4))
    assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
    assert(List.drop(List(1, 2, 3, 4), 3) == List(4))
    assert(List.drop(List(1, 2, 3, 4), 4) == Nil)
    assert(List.drop(List(1, 2, 3, 4), 5) == Nil)
  }

  test("EXERCISE 3.5 dropWhile") {
    def f(a: Int) = a < 3
    assert(List.dropWhile(Nil, f) == Nil)
    assert(List.dropWhile(List(1, 2, 3, 4), f) == List(3, 4))
    assert(List.dropWhile(List(2, 3, 4), f) == List(3, 4))
    assert(List.dropWhile(List(3, 4), f) == List(3, 4))
    assert(List.dropWhile(List(4), f) == List(4))
  }

  test("List 3-2") {
    assert(List.append(Nil, Nil) == Nil)
    assert(List.append(List(1, 2), Nil) == List(1, 2))
    assert(List.append(Nil, List(1, 2)) == List(1, 2))
    assert(List.append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4))
  }
}
