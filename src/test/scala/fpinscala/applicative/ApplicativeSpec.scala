package fpinscala.applicative

import org.scalatest.FunSuite

class ApplicativeSpec extends FunSuite {

  test("EXERCISE 12.5 eitherMonad") {
    import Monad.eitherMonad

    assert(eitherMonad.unit(1) == Right(1))

    assert(eitherMonad.flatMap(Right(1))(a => Right(a * 2)) == Right(2))
    assert(eitherMonad.flatMap(Left(1))(a => a) == Left(1))
  }

  test("EXERCISE 12.6 validationApplicative") {
    import Applicative.validationApplicative

    assert(validationApplicative.unit(1) == Success(1))

    assert(validationApplicative.map2(Success(1), Success(2))((a, b) => a + b) == Success((3)))
    assert(validationApplicative.map2(Failure(1, Vector(2, 3)), Failure(4, Vector(5, 6)))((a, b) => (a, b)) ==
      Failure(1, Vector(2, 3, 4, 5, 6)))

    assert(validationApplicative.map2(Success(1), Failure(4, Vector(5, 6)))((a, b) => (a, b)) ==
      Failure(4, Vector(5, 6)))

    assert(validationApplicative.map2(Failure(1, Vector(2, 3)), Success(4))((a, b) => (a, b)) ==
      Failure(1, Vector(2, 3)))
  }

  test("List 12-7") {
    import Applicative._

    assert(validName("taro") == Success("taro"))
    assert(validName("") == Failure("Name cannot be empty"))

    import java.text._
    assert(validBirthdate("2018-06-11") == Success((new SimpleDateFormat("yyyy-MM-dd").parse("2018-06-11"))))
    assert(validBirthdate("20180611") == Failure("Birthdate must be in the form yyyy-MM-dd"))

    assert(validPhone("1234567890") == Success("1234567890"))
    assert(validPhone("123456789") == Failure("Phone bumber must be 10 digits"))

    val s1 = validWebForm("taro", "2018-06-11", "1234567890")
    val a1 = WebForm("taro", (new SimpleDateFormat("yyyy-MM-dd")).parse("2018-06-11"), "1234567890")
    assert(s1 == Success(a1))

    val s2 = validWebForm("", "2018-06-11", "1234567890")
    assert(s2 == Failure("Name cannot be empty", Vector()))

    val s3 = validWebForm("", "20180611", "1234567890")
    assert(s3 == Failure("Name cannot be empty", Vector("Birthdate must be in the form yyyy-MM-dd")))

    val s4 = validWebForm("", "20180611", "123456789")
    assert(s4 == Failure(
      "Name cannot be empty",
      Vector("Birthdate must be in the form yyyy-MM-dd", "Phone bumber must be 10 digits")
    ))
  }

  test("EXERCISE 12.3 map3") {
    import Applicative._
    val o = optionApplicative
    assert(o.map3(Some(1), Some(2), Some(3))((a, b, c) => a + b + c) == Some(6))
  }

  test("EXERCISE 12.3 map4") {
    import Applicative._
    val o = optionApplicative
    assert(o.map4(Some(1), Some(2), Some(3), Some(4))((a, b, c, d) => a + b + c + d) == Some(10))
  }

  test("assoc") {
    import Applicative._
    val o = optionApplicative
    assert(o.assoc(Some(1), (None, Some(3))) == ((Some(1), None), Some(3)))
  }

  test("EXERCISE 12.8 product") {
    import Applicative._
    val g = optionApplicative
    val p = optionApplicative.product(g)
    assert(p.unit(1) == (Some(1), Some(1)))

    val f = (a: Int) => a.toString
    val a = p.apply((Some(f), Some(f)))((Some(1), Some(2)))
    assert(a == (Some("1"), Some("2")))
  }

  test("EXERCISE 12.9 compose") {
    import Applicative._

    val c = optionApplicative.compose(optionApplicative)
    assert(c.unit(1) == Some(Some(1)))
    assert(c.map2(c.unit(1), c.unit(2))((a, b) => a + b) == Some(Some(3)))
  }

  test("listApplicative") {
    import Applicative._

    val l = listApplicative
    assert(l.unit(1) == List(1))
    assert(l.map2(List(1, 2, 3), List(4, 5))((a, b) => a + b) == List(5, 7))
  }
}
