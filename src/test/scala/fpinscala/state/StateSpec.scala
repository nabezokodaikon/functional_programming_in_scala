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

  test("EXERCISE 6.6 map2 randIntDouble") {
    import RNG.randIntDouble
    val rng = SimpleRNG(42)
    val ((int1, double1), rng2) = randIntDouble(rng)
    val ((int2, double2), rng3) = randIntDouble(rng2)
    val ((int3, double3), _) = randIntDouble(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)

  }

  test("EXERCISE 6.6 map2 randDoubleInt") {
    import RNG.randDoubleInt
    val rng = SimpleRNG(42)
    val ((double1, int1), rng2) = randDoubleInt(rng)
    val ((double2, int2), rng3) = randDoubleInt(rng2)
    val ((double3, int3), _) = randDoubleInt(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)
  }

  test("EXERCISE 6.7 sequence") {
    import RNG.intsViaSequence
    assert(intsViaSequence(3)(SimpleRNG(42)) != intsViaSequence(3)(SimpleRNG(41)))
    assert(intsViaSequence(3)(SimpleRNG(42)) == intsViaSequence(3)(SimpleRNG(42)))
  }

  test("nonNegativeLessThan") {
    import RNG.nonNegativeLessThan
    assert(nonNegativeLessThan(3)(SimpleRNG(42)) != nonNegativeLessThan(3)(SimpleRNG(41)))
    assert(nonNegativeLessThan(3)(SimpleRNG(42)) != nonNegativeLessThan(Int.MaxValue)(SimpleRNG(42)))
    assert(nonNegativeLessThan(3)(SimpleRNG(42)) == nonNegativeLessThan(3)(SimpleRNG(42)))
  }

  test("nonNegativeLessThanViaFlatMap") {
    import RNG.nonNegativeLessThanViaFlatMap
    assert(nonNegativeLessThanViaFlatMap(3)(SimpleRNG(42)) != nonNegativeLessThanViaFlatMap(3)(SimpleRNG(41)))
    assert(nonNegativeLessThanViaFlatMap(3)(SimpleRNG(42)) != nonNegativeLessThanViaFlatMap(Int.MaxValue)(SimpleRNG(42)))
    assert(nonNegativeLessThanViaFlatMap(3)(SimpleRNG(42)) == nonNegativeLessThanViaFlatMap(3)(SimpleRNG(42)))
  }

  test("EXERCISE 6.9 mapViaFlatMap") {
    import RNG.{ mapViaFlatMap, Rand }
    val r = SimpleRNG(42)
    val s: Rand[Int] = _.nextInt
    val s2 = mapViaFlatMap(s)(a => a.toString)
    assert(s(r)._1.toString == s2(r)._1)
  }

  test("EXERCISE 6.9 map2ViaFlatMap randIntDouble") {
    import RNG.randIntDoubleViaFlatMap
    val rng = SimpleRNG(42)
    val ((int1, double1), rng2) = randIntDoubleViaFlatMap(rng)
    val ((int2, double2), rng3) = randIntDoubleViaFlatMap(rng2)
    val ((int3, double3), _) = randIntDoubleViaFlatMap(rng)
    assert(int1 != int2)
    assert(double1 != double2)
    assert(int1 == int3)
    assert(double1 == double3)
  }

  test("EXERCISE 6.10 unit") {
    import State.unit
    import RNG.SimpleRNG
    val r = SimpleRNG(42)
    assert(unit(r).run(r) == (r, r))
  }

  test("EXERCISE 6.10 map") {
    val s = State.unit("-")
    val (a1, _) = State.unit(1).map(a => a.toString).run(s)
    val (a2, _) = State.unit("1").run(s)
    assert(a1 == a2)
  }

  test("EXERCISE 6.10 map2") {
    val (c, _) = (State.unit(1): State[Int, Int]).map2(State.unit(2): State[Int, Int])((a, b) => a + b).run(9)
    assert(c == 3)
  }

  test("EXERCISE 6.10 sequence") {
    import State.{ sequence, unit }
    val l = List[State[String, Int]](
      State.unit(1),
      State.unit(2),
      State.unit(3)
    )
    assert(sequence(l).run("state") == unit(List(1, 2, 3)).run("state"))
  }

  test("List 6-10 modify") {
    import State.modify
    val r = modify[Int](a => a * a)
    assert(r.run(3) == ((), 9))
  }

  test("EXERCISE 6.11") {
    val m = Machine(true, 5, 10)
    val r = Candy.simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    val s = r.run(m)
    assert(s._1._1 == 14)
    assert(s._1._2 == 1)
  }
}
