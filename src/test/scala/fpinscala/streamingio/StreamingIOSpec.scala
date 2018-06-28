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
}
