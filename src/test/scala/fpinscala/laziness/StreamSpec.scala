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

  test("EXERCISE 5.5 takeWhileViaFoldRight") {
    import Stream.empty
    assert(Stream[Int]().takeWhileViaFoldRight(a => a < 3) == empty)
    assert(Stream(3, 2, 1).takeWhileViaFoldRight(a => a < 3) == empty)
    assert(Stream(1, 2, 3).takeWhileViaFoldRight(a => a < 3).toList == List(1, 2))
  }

  test("EXERCISE 5.6 headOptionViaFoldRight") {
    assert(Stream[Int]().headOptionViaFoldRight == None)
    assert(Stream(1).headOptionViaFoldRight == Some(1))
    assert(Stream(1, 2).headOptionViaFoldRight == Some(1))
  }

  test("EXERCISE 5.7 map") {
    assert(Stream[Int]().map(a => a.toString) == Stream[String]())
    assert(Stream(1, 2, 3).map(a => a.toString).toList == List("1", "2", "3"))
  }

  test("EXERCISE 5.7 filter") {
    assert(Stream[Int]().filter(a => a % 2 == 0).toList == List[Int]())
    assert(Stream(1, 3, 5).filter(a => a % 2 == 0).toList == List[Int]())
    assert(Stream(1, 2, 3, 4).filter(a => a % 2 == 0).toList == List(2, 4))
  }

  test("EXERCISE 5.7 append") {
    assert(Stream[Int]().append(Stream[Int]()).toList == List[Int]())
    assert(Stream(1, 2, 3).append(Stream[Int]()).toList == List(1, 2, 3))
    assert(Stream[Int]().append(Stream(4, 5)).toList == List(4, 5))
    assert(Stream(1, 2, 3).append(Stream(4, 5)).toList == List(1, 2, 3, 4, 5))
  }

  test("EXERCISE 5.7 flatMap") {
    import Stream.empty
    assert(empty[Int].flatMap(_ => empty[Int]) == empty[Int])
    assert(Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList == List(1, 1, 2, 2, 3, 3))
    assert(Stream(Stream(1, 2), Stream(3, 4)).flatMap(i => i).toList == List(1, 2, 3, 4))
  }

  test("find") {
    assert(Stream[Int]().find(a => a % 2 == 0) == None)
    assert(Stream(1, 3, 5).find(a => a % 2 == 0) == None)
    assert(Stream(1, 2, 3, 4).find(a => a % 2 == 0) == Some(2))
  }

  test("ones") {
    import Stream.ones
    assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
    assert(ones.exists(_ % 2 != 0) == true)
    assert(ones.map(_ + 1).exists(_ % 2 == 0) == true)
  }
}
