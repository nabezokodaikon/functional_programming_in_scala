package fpinscala.testing

import fpinscala.state.{ RNG, State }

sealed trait Prop {
  import Prop.{ FailedCase, SuccessCount }

  // def check: Boolean

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  // EXERCISE 8.3
  // def &&(p: Prop): Prop = new Prop {
  // def check = Prop.this.check && p.check
  // }
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int

}

case class Gen[+A](sample: State[RNG, A])

object Gen {

  // EXERCISE 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegariveInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))
}

// object Gen {

// def listOf[A](a: Gen[A]): Gen[List[A]]
// def listOf[A](n: Int, a: Gen[A]): Gen[List[A]]

// def forAll[A](a: Gen[A])(f: A => Boolean): Prop
// }
