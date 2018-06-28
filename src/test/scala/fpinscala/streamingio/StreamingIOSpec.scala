package fpinscala.streamingio

import org.scalatest.FunSuite

class StreamingIOSpec extends FunSuite {

  test("List 15-1 linesGt5") {
    import ImperativeAndLazyIO._
    assert(linesGt5("""resource/List15-1_01.txt""").run == true)
    assert(linesGt5("""resource/List15-1_02.txt""").run == false)
  }

  test("Examples 01") {
    val lines: Stream[String] = Stream("a", "b", "a", " ", "c", "a")
    assert(lines.zipWithIndex.exists(_._2 + 1 >= 4) == true)
    assert(lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 4) == true)
    assert(lines.filter(!_.trim.isEmpty).take(4).map(_.head).indexOfSlice("c".toList) == 3)
  }

  test("List 15-2 lines") {
    import ImperativeAndLazyIO._
    val res = lines("""resource/List15-1_01.txt""")
    assert(res.run(0) == "1")
    assert(res.run(1) == "2")
  }

  test("List 15-4 Process.apply") {
    import SimpleStreamTransducers.Process._

    {
      val halt = Halt()
      println("### Halt")
      println(halt)
    }

    {
      val emit = Emit[Int, String]("1")
      println("### Emit")
      println(emit)
      println(emit.head)
      println(emit.tail)
    }

    {
      val await = Await[Int, String]((recv: Option[Int]) => Emit("1"))
      println("### Await")
      println(await.recv)
      println(await.recv(None))
      println(await.recv(None)(Stream(1, 2, 3)))
    }
  }

  test("List 15-5 liftOne") {
    import SimpleStreamTransducers.Process._

    {
      val p = liftOne((x: Int) => x * 10)
      val xs = p(Stream(1, 2, 3)).toList
      assert(xs == List(10))
    }

    {
      val p = liftOne((None: Int) => None)
      val xs = p(Stream(1, 2, 3)).toList
      assert(xs == List(1))
    }
  }

  test("List 15-6 repeat") {
    import SimpleStreamTransducers.Process._
    val p = liftOne((x: Int) => x * 10)
    val p2 = p.repeat
    val xs = p2(Stream(1, 2, 3)).toList
    assert(xs == List(10, 20, 30))
  }

  test("List 15-7 lift") {
    import SimpleStreamTransducers.Process._
    val p = lift((x: Int) => x * 10)
    val xs = p(Stream(1, 2, 3)).toList
    assert(xs == List(10, 20, 30))
  }

  test("List 15-8 filter") {
    import SimpleStreamTransducers.Process._
    val even = filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1, 2, 3, 4)).toList
    assert(evens == List(2, 4))
  }

  test("これまでに検出された値の累積合計を出力する。") {
    import SimpleStreamTransducers.Process._
    val s = sum(Stream(1.0, 2.0, 3.0, 4.0)).toList
    assert(s == List(1.0, 3.0, 6.0, 10.0))
  }

  test("EXERCISE 15.1 take") {
    import SimpleStreamTransducers.Process._
    val p = take[Int](2)
    val r = p(Stream(1, 2, 3, 4)).toList
    assert(r == List(1, 2, 3))
  }

  test("EXERCISE 15.1 drop") {
    import SimpleStreamTransducers.Process._
    val p = drop[Int](2)
    val r = p(Stream(1, 2, 3, 4)).toList
    assert(r == List(3, 4))
  }

  test("EXERCISE 15.1 takeWhile") {
    import SimpleStreamTransducers.Process._
    val p = takeWhile[Int](i => i < 3)
    val r = p(Stream(1, 2, 3, 4)).toList
    assert(r == List(1, 2))
  }

  test("EXERCISE 15.1 dropWhile") {
    import SimpleStreamTransducers.Process._
    val p = dropWhile[Int](i => i < 3)
    val r = p(Stream(1, 2, 3, 4)).toList
    assert(r == List(3, 4))
  }

  test("EXERCISE 15.2 count") {
    import SimpleStreamTransducers.Process._
    val r = count(Stream("a", "b", "c", "d")).toList
    assert(r == List(1, 2, 3, 4))
  }

  test("EXERCISE 15.3 mean") {
    import SimpleStreamTransducers.Process._
    val r = mean(Stream(1.0, 2.0, 3.0, 4.0)).toList
    assert(r == List(1, 1.5, 2, 2.5))
  }

  test("EXERCISE 15.4 sumViaLoop") {
    import SimpleStreamTransducers.Process._
    val s = sumViaLoop(Stream(1.0, 2.0, 3.0, 4.0)).toList
    assert(s == List(1.0, 3.0, 6.0, 10.0))
  }

  test("EXERCISE 15.4 countViaLoop") {
    import SimpleStreamTransducers.Process._
    val r = countViaLoop(Stream("a", "b", "c", "d")).toList
    assert(r == List(1, 2, 3, 4))
  }
}
