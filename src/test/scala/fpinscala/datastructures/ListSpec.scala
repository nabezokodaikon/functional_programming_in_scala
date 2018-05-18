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

  test("EXERCISE 3.6 init") {
    import List.init
    assertThrows[RuntimeException] {
      init(Nil)
    }
    assert(init(List(1)) == Nil)
    assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))
  }

  test("List 3-3 sum2") {
    import List.sum2
    assert(sum2(Nil) == 0)
    assert(sum2(List(1, 2, 3, 4)) == 10)
  }

  test("List 3-3 product2") {
    import List.product2
    assert(product2(Nil) == 1.0)
    assert(product2(List(1, 2, 0, 4)) == 0)
    assert(product2(List(1, 2, 3, 4)) == 24)
  }

  test("EXERCISE 3.8") {
    import List.foldRight
    assert(foldRight(Nil, Nil: List[Int])(Cons(_, _)) == Nil)
    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  test("EXERCISE 3.9") {
    import List.length
    assert(length(Nil) == 0)
    assert(length(List("a")) == 1)
    assert(length(List("a", "b")) == 2)
    assert(length(List("a", "b", "c")) == 3)
  }

  test("EXERCISE 3.10 foldLeft") {
    import List.foldLeft
    def sum(l: List[Int]) = {
      foldLeft(l, 0)((b, a) => b + a)
    }

    assert(sum(List(1, 2, 3, 4)) == 10)
  }

  test("EXERCISE 3.11 sum") {
    import List.sumByFoldLeft
    assert(sumByFoldLeft(Nil) == 0)
    assert(sumByFoldLeft(List(1)) == 1)
    assert(sumByFoldLeft(List(1, 2, 3, 4)) == 10)
  }

  test("EXERCISE 3.11 product") {
    import List.productByFoldLeft
    assert(productByFoldLeft(Nil) == 1.0)
    assert(productByFoldLeft(List(1)) == 1)
    assert(productByFoldLeft(List(1, 2, 3, 4)) == 24)
    assert(productByFoldLeft(List(1, 2, 0, 4)) == 0.0)
  }

  test("EXERCISE 3.11 length") {
    import List.lengthByFoldLeft
    assert(lengthByFoldLeft(Nil) == 0)
    assert(lengthByFoldLeft(List("a")) == 1)
    assert(lengthByFoldLeft(List("a", "b")) == 2)
    assert(lengthByFoldLeft(List("a", "b", "c")) == 3)
  }

  test("EXERCISE 3.12 reverse") {
    import List.reverse
    assert(reverse(Nil) == Nil)
    assert(reverse(List(1)) == List(1))
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  test("EXERCISE 3.13 foldLeft via foldRight") {
    import List.foldLeftViaFoldRight

    def reverse[A](l: List[A]): List[A] =
      foldLeftViaFoldRight(l, List[A]())((acc, h) => Cons(h, acc))

    assert(reverse(Nil) == Nil)
    assert(reverse(List(1)) == List(1))
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }
}
