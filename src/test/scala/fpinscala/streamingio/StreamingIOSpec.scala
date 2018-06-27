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
}

