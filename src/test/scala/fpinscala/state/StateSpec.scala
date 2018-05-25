package fpinscala.state

import org.scalatest.FunSuite

class StateSpec extends FunSuite {

  import RNG.SimpleRNG

  test("List 6-3 nextInt") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    assert(n1 == rng.nextInt._1)
    assert(n2 == rng2.nextInt._1)
  }

  test("EXERCISE 6.1 nonNegariveInt") {
    import RNG.nonNegariveInt
    val rng = SimpleRNG(42)
    val (n2, rng2) = nonNegariveInt(rng)
    val (n3, rng3) = nonNegariveInt(rng2)
    assert(n2 > 0)
    assert(n2 != n3)
    assert(n2 == nonNegariveInt(rng)._1)
  }

  test("EXERCISE 6.2 double") {
    import RNG.double
    val rng = SimpleRNG(42)
    val (n1, rng2) = double(rng)
    val (n2, rng3) = double(rng2)
    assert(n1 < 1)
    assert(n1 == double(rng)._1)
    assert(n1 != n2)
  }

  test("EXERCISE 6.3 intDouble") {
    import RNG.intDouble
    val rng = SimpleRNG(42)
    val ((int1, double1), rng2) = intDouble(rng)
    val ((int2, double2), rng3) = intDouble(rng2)
    val ((int3, double3), _) = intDouble(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)
  }

  test("EXERCISE 6.3 doubleInt") {
    import RNG.doubleInt
    val rng = SimpleRNG(42)
    val ((double1, int1), rng2) = doubleInt(rng)
    val ((double2, int2), rng3) = doubleInt(rng2)
    val ((double3, int3), _) = doubleInt(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)
  }

  test("EXERCISE 6.4 double3") {
    import RNG.double3
    val rng = SimpleRNG(42)
    val ((d1a, d2a, d3a), rng2) = double3(rng)
    val ((d1b, d2b, d3b), rng3) = double3(rng2)
    val ((d1c, d2c, d3c), rng4) = double3(rng)
    assert(d1a != d1b && d2a != d2b && d3a != d3b)
    assert(d1a == d1c && d2a == d2c && d3a == d3c)
  }

  test("EXERCISE 6.4 ints") {
    import RNG.ints
    val rng = SimpleRNG(42)
    val (l, r1) = ints(3)(rng)
    assert(ints(3)(r1)._1 != l)
    assert(ints(3)(rng)._1 == l)
  }

  test("List 6-6 unit") {
    import RNG.unit
    val r = SimpleRNG(42)
    assert(unit(r)(r) == (r, r))
  }

  test("List 6-7 map") {
    import RNG.{ map, Rand }
    val r = SimpleRNG(42)
    val s: Rand[Int] = _.nextInt
    val s2 = map(s)(a => a.toString)
    assert(s(r)._1.toString == s2(r)._1)
  }

  test("List 6-7 nonNegativeEven") {
    import RNG.nonNegativeEven
    val r = SimpleRNG(42)
    assert(nonNegativeEven(r)._1 % 2 == 0)
  }

  test("EXERCISE 6.5 doubleViaMap") {
    import RNG.doubleViaMap
    val rng = SimpleRNG(42)
    val (n1, rng2) = doubleViaMap(rng)
    val (n2, rng3) = doubleViaMap(rng2)
    assert(n1 < 1)
    assert(n1 == doubleViaMap(rng)._1)
    assert(n1 != n2)
  }
}
