package fpinscala.testing

import org.scalatest.FunSuite
import fpinscala.state.RNG.SimpleRNG

class GenSpec extends FunSuite {

  test("EXERCISE 8.4 choose") {
    val rng = SimpleRNG(1)
    val a = Gen.choose(2, 5).sample.run(rng)._1
    assert(2 <= a && a <= 5)
  }

  test("EXERCISE 8.5 unit") {
    val rng = SimpleRNG(1)
    val a = Gen.unit(2).sample.run(rng)._1
    assert(a == 2)
  }

  test("EXERCISE 8.5 boolean") {
    val rng = SimpleRNG(1)
    val a = Gen.boolean.sample.run(rng)._1
    assert(a == true || a == false)
  }
}
