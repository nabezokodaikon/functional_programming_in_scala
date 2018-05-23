package fpinscala.laziness

import org.scalatest.FunSuite

class StreamSpec extends FunSuite {

  test("List 5-2 headOption") {
    assert(Stream[Int]().headOption == None)
    assert(Stream(1).headOption == Some(1))
    assert(Stream(1, 2).headOption == Some(1))
  }

  test("EXERCISE 5.1 toList") {
    assert(Stream[Int]().toList == List[Int]())
    assert(Stream(1).toList == List(1))
    assert(Stream(1, 2).toList == List(1, 2))
  }

  test("EXERCISE 5.1 toList_2") {
    assert(Stream[Int]().toList_2 == List[Int]())
    assert(Stream(1).toList_2 == List(1))
    assert(Stream(1, 2).toList_2 == List(1, 2))
  }

  test("EXERCISE 5.2 take") {
    import Stream._
    assert(Stream[Int]().take(0) == empty)
    assert(Stream[Int]().take(1) == empty)
    assert(Stream(1).take(0) == empty)
    assert(Stream(1).take(1).toList == List(1))
    assert(Stream(1, 2, 3).take(0) == empty)
    assert(Stream(1, 2, 3).take(1).toList == List(1))
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).take(3).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3, 4).take(3).toList == List(1, 2, 3))
  }

  test("EXERCISE 5.2 drop") {
    import Stream.empty
    assert(Stream[Int]().drop(0) == empty)
    assert(Stream[Int]().drop(1) == empty)
    assert(Stream(1).drop(0).toList == List(1))
    assert(Stream(1, 2).drop(1).toList == List(2))
    assert(Stream(1, 2, 3).drop(1).toList == List(2, 3))
    assert(Stream(1, 2, 3).drop(3) == empty)
    assert(Stream(1, 2, 3).drop(4) == empty)
  }

  test("EXERCISE 5.3 takeWhile") {
    import Stream.empty
    assert(Stream[Int]().takeWhile(a => a < 3) == empty)
    assert(Stream(3, 2, 1).takeWhile(a => a < 3) == empty)
    assert(Stream(1, 2, 3).takeWhile(a => a < 3).toList == List(1, 2))
  }

  test("exists") {
    assert(Stream[Int]().exists(a => a > 1) == false)
    assert(Stream(3, 2, 1).exists(a => a > 1) == true)
    assert(Stream(1, 2, 3).exists(a => a > 3) == false)
  }

  test("List 5-4 existsViaFoldRight") {
    assert(Stream[Int]().existsViaFoldRight(a => a > 1) == false)
    assert(Stream(3, 2, 1).existsViaFoldRight(a => a > 1) == true)
    assert(Stream(1, 2, 3).existsViaFoldRight(a => a > 3) == false)
  }

  test("EXERCISE 5.4 forAll") {
    // assert(Stream[Int]().forAll(a => a % 2 == 0) == false)
    assert(Stream(1, 2, 4).forAll(a => a % 2 == 0) == false)
    assert(Stream(2, 3, 4).forAll(a => a % 2 == 0) == false)
    assert(Stream(2, 4, 5).forAll(a => a % 2 == 0) == false)
    assert(Stream(2, 4, 6).forAll(a => a % 2 == 0) == true)
  }
}
