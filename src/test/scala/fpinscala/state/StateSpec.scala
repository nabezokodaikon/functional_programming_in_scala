package fpinscala.state

import org.scalatest.FunSuite

class StateSpec extends FunSuite {

  test("List 6-3 nextInt") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    assert(n1 == rng.nextInt._1)
    assert(n2 == rng2.nextInt._1)
  }

  test("EXERCISE 6.1 nonNegariveInt") {
    val rng = SimpleRNG(42)
    val (n2, rng2) = rng.nonNegariveInt(rng)
    val (n3, rng3) = rng2.nonNegariveInt(rng2)
    assert(n2 > 0)
    assert(n2 != n3)
    assert(n2 == rng.nonNegariveInt(rng)._1)
  }

  test("EXERCISE 6.2 double") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.double(rng)
    val (n2, rng3) = rng2.double(rng2)
    assert(n1 < 1)
    assert(n1 == rng.double(rng)._1)
    assert(n1 != n2)
  }

}
