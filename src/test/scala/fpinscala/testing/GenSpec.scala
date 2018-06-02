package fpinscala.testing

import org.scalatest.FunSuite
import fpinscala.state.RNG.SimpleRNG
import fpinscala.state.State

class GenSpec extends FunSuite {

  test("testRunner") {
    Main.testRunner()
  }

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

  test("EXERCISE 8.10 unsized") {
    val g = Gen.unit(1)
    val sg = g.unsized
    assert(sg.forSize(1) == g)
  }

  test("map") {
    val rng = SimpleRNG(42)
    val g = Gen.unit(1)
    assert(g.map(_.toString).sample.run(rng)._1 == "1")
  }

  test("map2") {
    val rng = SimpleRNG(42)
    val ga = Gen.unit(1)
    val gb = Gen.unit(2)
    assert(ga.map2(gb)((a, b) => a + b).sample.run(rng)._1 == 3)
  }

  test("`**` function") {
    val rng = SimpleRNG(42)
    val ga = Gen.unit(1)
    val gb = Gen.unit(2)
    assert((ga ** gb).sample.run(rng)._1 == (1, 2))
  }

  test("EXERCISE 8.11 apply") {
    val rng = SimpleRNG(42)
    val g = Gen.unit(1)
    val s = SGen((a: Int) => Gen.unit(a))
    assert(s.apply(1).sample.run(rng)._1 == g.sample.run(rng)._1)
  }

  test("EXERCISE 8.11 map") {
    val rng = SimpleRNG(42)
    val s1 = Gen.unit(5).unsized
    assert(s1.map(_.toString).apply(1).sample.run(rng)._1 == "5")
  }

  test("EXERCISE 8.11 flatMap") {
    val rng = SimpleRNG(42)
    val s = Gen.unit(5).unsized
    assert(s.flatMap(a => Gen.unit(a.toString).unsized).apply(1).sample.run(rng)._1 == "5")
  }

  test("EXERCISE 8.11 `**`") {
    val rng = SimpleRNG(42)
    val sa = Gen.unit(5).unsized
    val sb = Gen.unit(6).unsized
    assert((sa ** sb).apply(1).sample.run(rng)._1 == (5, 6))
  }

  test("EXERCISE 8.12 listOf") {
    val rng = SimpleRNG(1)
    val g = Gen.unit(3)
    val s = Gen.unit(1).unsized
    val l = s.listOf(g)
    assert(l.forSize(5).sample.run(rng)._1 == List(3, 3, 3, 3, 3))
  }
}
