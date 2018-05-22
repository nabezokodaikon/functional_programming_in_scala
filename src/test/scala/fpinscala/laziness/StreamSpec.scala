package fpinscala.laziness

import org.scalatest.FunSuite

class StreamSpec extends FunSuite {

  test("List 5-2 headOption") {
    assert(Stream[Int]().headOption == None)
    assert(Stream(1).headOption == Some(1))
    assert(Stream(1, 2).headOption == Some(1))
  }
}
