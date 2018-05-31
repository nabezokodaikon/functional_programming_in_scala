package fpinscala.testing

import org.scalatest.FunSuite
import fpinscala.state.RNG.SimpleRNG

class GenSpec extends FunSuite {

  test("EXERCISE 8.4 choose") {
    val rng = SimpleRNG(1)
    val a = Gen.choose(2, 5).sample.run(rng)._1
    assert(2 <= a && a <= 5)
  }
}
