package fpinscala.gettingstarted

import org.scalatest.FunSuite

class GettingStartedSpec extends FunSuite {
  test("abs fail") {
    assert(MyModule.abs(1) !== -1)
    assert(MyModule.abs(-1) !== -1)
  }

  test("abs success") {
    assert(MyModule.abs(1) === 1)
    assert(MyModule.abs(0) === 0)
    assert(MyModule.abs(-1) === 1)
  }
}
