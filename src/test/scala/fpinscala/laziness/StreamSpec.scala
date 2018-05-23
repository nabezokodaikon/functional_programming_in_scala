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
}
