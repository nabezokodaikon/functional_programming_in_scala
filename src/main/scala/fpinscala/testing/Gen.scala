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
  type SuccessCount = Int
  type FailedCase = String
  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, sucesses: SuccessCount) extends Result {
    def isFalsified = true
  }
}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    flatMap(a => Gen.unit(f(a)))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))
}

object Gen {

  // EXERCISE 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegariveInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }
}

// object Gen {

// def listOf[A](a: Gen[A]): Gen[List[A]]
// def listOf[A](n: Int, a: Gen[A]): Gen[List[A]]

// def forAll[A](a: Gen[A])(f: A => Boolean): Prop
// }
