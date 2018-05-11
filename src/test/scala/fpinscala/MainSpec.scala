package fpinscala

import org.scalatest.FunSuite

class MainSpec extends FunSuite {
  test("helloWorld") {
    assert(Main.helloWorld("nabezokodaikokn") == "Hello nabezokodaikokn!")
  }

  test("helloWorld Failed") {
    assert(Main.helloWorld("nabezokodaikokn") != "Hello")
  }
}
