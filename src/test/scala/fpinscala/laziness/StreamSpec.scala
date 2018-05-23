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
}
