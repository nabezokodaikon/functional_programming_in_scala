package fpinscala.applicative

import org.scalatest.FunSuite

class ApplicativeSpec extends FunSuite {

  test("EXERCISE 12.5 eitherMonad") {
    import Monad.eitherMonad

    assert(eitherMonad.unit(1) == Right(1))

    assert(eitherMonad.flatMap(Right(1))(a => Right(a * 2)) == Right(2))
    assert(eitherMonad.flatMap(Left(1))(a => a) == Left(1))
  }
}
