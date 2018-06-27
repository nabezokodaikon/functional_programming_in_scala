package fpinscala.streamingio

import org.scalatest.FunSuite

class StreamingIOSpec extends FunSuite {

  test("List 15-1 linesGt5") {
    import ImperativeAndLazyIO._
    assert(linesGt5("""resource/List15-1_01.txt""").run == true)
    assert(linesGt5("""resource/List15-1_02.txt""").run == false)
  }
}

