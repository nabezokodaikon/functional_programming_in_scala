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
}
