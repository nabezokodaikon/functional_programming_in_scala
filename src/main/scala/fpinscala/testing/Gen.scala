package fpinscala.testing

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object ScalaCheck extends App {

  {
    println("List 8-1")
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop =
      forAll(intList)(ns => ns.reverse.reverse == ns) &&
        forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    prop.check
    val failingProp = forAll(intList)(ns => ns.reverse == ns)
    failingProp.check
  }

  {
    println("EXERCISE 8.1")
    val a = forAll { l1: List[Int] =>
      l1.sum == l1.reverse.sum
    }
    a.check

    val bl = Gen.listOf(Gen.choose(0, 10))
    val b = forAll(bl) { ns =>
      ns.sum == ns.reverse.sum
    }
    b.check

    val cl = Gen.listOf(Gen.choose(10, 10))
    val c = forAll(cl) { ns =>
      ns.headOption match {
        case Some(h) => h * ns.size == ns.sum
        case _ => ns.size == ns.sum
      }
    }
    c.check
  }
}
