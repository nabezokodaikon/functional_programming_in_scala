package fpinscala.testing

import org.scalatest.FunSuite
import fpinscala.state.RNG.SimpleRNG
import fpinscala.state.State

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

  test("EXERCISE 8.5 listOfN") {
    val rng = SimpleRNG(1)
    val g = Gen.unit(3)
    val a = Gen.listOfN(5, g)
    assert(a.sample.run(rng)._1 == List(3, 3, 3, 3, 3))
  }

  test("EXERCISE 8.6 flatMap") {
    val rng = SimpleRNG(1)
    val g = Gen.unit(3)
    val b = g.flatMap(a => Gen.unit(a.toString))
    assert(b.sample.run(rng)._1 == "3")
  }

  test("EXERCISE 8.6 listOfN") {
    val rng = SimpleRNG(1)
    val g = Gen.unit(3)
    val s = Gen.unit(5)
    val l = g.listOfN(s)
    assert(l.sample.run(rng)._1 == List(3, 3, 3, 3, 3))
  }

  test("EXERCISE 8.7 union") {
    val rng1 = SimpleRNG(1)
    val rng2 = SimpleRNG(2)
    val g1 = Gen.unit(3)
    val g2 = Gen.unit(5)

    assert(Gen.union(g1, g2).sample.run(rng1)._1 == g1.sample.run(rng1)._1)
    assert(Gen.union(g1, g2).sample.run(rng2)._1 != g1.sample.run(rng1)._1)

    assert(Gen.union(g1, g2).sample.run(rng2)._1 == g2.sample.run(rng2)._1)
    assert(Gen.union(g1, g2).sample.run(rng1)._1 != g2.sample.run(rng1)._1)
  }

  test("EXERCISE 8.8 weighted") {
    val rng = SimpleRNG(1)

    {
      val g1 = (Gen.unit(2), 0.1)
      val g2 = (Gen.unit(3), 0.5)

      assert(Gen.weighted(g1, g2).sample.run(rng)._1 == g1._1.sample.run(rng)._1)
      assert(Gen.weighted(g1, g2).sample.run(rng)._1 != g2._1.sample.run(rng)._1)
    }

    {
      val g1 = (Gen.unit(2), 0.0)
      val g2 = (Gen.unit(3), 0.5)

      assert(Gen.weighted(g1, g2).sample.run(rng)._1 != g1._1.sample.run(rng)._1)
      assert(Gen.weighted(g1, g2).sample.run(rng)._1 == g2._1.sample.run(rng)._1)
    }
  }
}
