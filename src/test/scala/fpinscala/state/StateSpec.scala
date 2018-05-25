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

  test("EXERCISE 6.3 intDouble") {
    val rng = SimpleRNG(42)
    val ((int1, double1), rng2) = rng.intDouble(rng)
    val ((int2, double2), rng3) = rng.intDouble(rng2)
    val ((int3, double3), _) = rng.intDouble(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)
  }

  test("EXERCISE 6.3 doubleInt") {
    val rng = SimpleRNG(42)
    val ((double1, int1), rng2) = rng.doubleInt(rng)
    val ((double2, int2), rng3) = rng.doubleInt(rng2)
    val ((double3, int3), _) = rng.doubleInt(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)
  }

  test("EXERCISE 6.4 double3") {
    val rng = SimpleRNG(42)
    val ((d1a, d2a, d3a), rng2) = rng.double3(rng)
    val ((d1b, d2b, d3b), rng3) = rng.double3(rng2)
    val ((d1c, d2c, d3c), rng4) = rng.double3(rng)
    assert(d1a != d1b && d2a != d2b && d3a != d3b)
    assert(d1a == d1c && d2a == d2c && d3a == d3c)
  }

  test("EXERCISE 6.4 ints") {
    import State.ints
    val rng = SimpleRNG(42)
    val (l, r1) = ints(3)(rng)
    assert(ints(3)(r1)._1 != l)
    assert(ints(3)(rng)._1 == l)
  }
}
