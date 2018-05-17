package fpinscala.gettingstarted

import org.scalatest.FunSuite

class GettingStartedSpec extends FunSuite {
  test("main") {
    MyModule.main(Array[String]())
  }

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

  test("findFirst of Generic function") {
    import MyModule.findFirst

    assert(findFirst(Array(1, 2, 3), (i: Int) => i == 4) == -1)
    assert(findFirst(Array(1, 2, 3), (i: Int) => i == 3) == 2)

    assert(findFirst(Array("a", "b", "c"), (i: String) => i == "d") == -1)
    assert(findFirst(Array("a", "b", "c"), (i: String) => i == "c") == 2)
  }

  test("isSorted") {
    import MyModule.isSorted

    assert(isSorted(Array(2, 1, 3), (a: Int, b: Int) => a <= b) == false)
    assert(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a <= b) == true)

    assert(isSorted(Array("b", "a", "c"), (a: String, b: String) => a <= b) == false)
    assert(isSorted(Array("a", "b", "d"), (a: String, b: String) => a <= b) == true)
  }

  test("EXERCISE 2.3") {
    import MyModule.curry

    val p = curry((a: Int, b: String) => b.format(a))
    assert(p(10)("Value %d") == "Value 10")
  }

  test("EXERCISE 2.4") {
    import MyModule._

    {
      val p = uncurry((a: Int) => (b: String) => b.format(a))
      assert(p(11, "Value %d") == "Value 11")
    }

    {
      val f1 = curry((a: Int, b: String) => b.format(a))
      val f2 = uncurry(f1)
      assert(f2(12, "Value %d") == "Value 12")
    }

  }
}
