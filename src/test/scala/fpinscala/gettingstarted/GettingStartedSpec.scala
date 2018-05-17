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

  test("fibonacci") {
    import MyModule.fibonacci
    assert(fibonacci(0) == 0)
    assert(fibonacci(1) == 1)
    assert(fibonacci(2) == 1)
    assert(fibonacci(3) == 2)
    assert(fibonacci(4) == 3)
    assert(fibonacci(5) == 5)
  }

  test("findFirst") {
    import MyModule.findFirst
    assert(findFirst(Array[String](), "a") == -1)
    assert(findFirst(Array[String](), "c") == -1)
    assert(findFirst(Array("a", "b"), "c") == -1)
    assert(findFirst(Array("a", "b"), "b") == 1)
  }
}
