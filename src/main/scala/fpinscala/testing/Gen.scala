package fpinscala.testing

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object ScalaCheck extends App {

  {
    /*
     * List 8-1
     */
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop =
      forAll(intList)(ns => ns.reverse.reverse == ns) &&
        forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    prop.check
    val failingProp = forAll(intList)(ns => ns.reverse == ns)
    failingProp.check
  }

}
