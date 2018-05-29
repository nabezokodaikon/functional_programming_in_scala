package fpinscala.testing

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object SampleSpecification extends Properties("String") {
  // $ sbt testOnly fpinscala.testing.SampleSpecification

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a + b).length > a.length && (a + b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a + b + c).substring(a.length, a.length + b.length) == b
  }
}
