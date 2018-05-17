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

  test("factorial") {
    import MyModule.factorial
    assert(factorial(-1) === 1)
    assert(factorial(0) === 1)
    assert(factorial(1) === 1)
    assert(factorial(2) === 2)
    assert(factorial(3) === 6)
    assert(factorial(7) === 5040)
  }
}
