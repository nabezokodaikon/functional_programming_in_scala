package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.{ RNG, State }
import fpinscala.state.RNG.SimpleRNG
import Prop._

object Main extends App {

  val rng = SimpleRNG(1)

  {
    println("pair")
    val s = Gen.pair(1, 10)
    val a = s.sample.run(rng)
    println(a)
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // EXERCISE 8.9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
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

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  // ジェネレータを繰り返しサンプリングすることにより、A値の無限ストリームを生成
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: ${s}
      generated an exception: ${e.getMessage}
      stack trace:
      ${e.getStackTrace.mkString("\n")}"""

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (max, n, rng) =>
      // サイズごとに、この数のランダムケースを生成。
      val casesPerSize = (n + (max - 1)) / max

      // サイズごとにプロパティを1つ作成するが、プロパティの数がn個を超えないようにする。
      val props: Stream[Prop] = Stream.from(0).take((n.min(max)) + 1).map(i => forAll(g(i))(f))

      // すべてを1つのプロパティにまとめる。
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)

      prop.run(max, n, rng)
    }
}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    flatMap(a => Gen.unit(f(a)))

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
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

  def pair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    for {
      a <- choose(start, stopExclusive)
      b <- choose(start, stopExclusive)
    } yield (a, b)
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(size: Int): Gen[A] = forSize(size)

  def map[B](f: A => B): SGen[B] =
    SGen { forSize(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val forSize2: Int => Gen[B] =
      size => {
        forSize(size) flatMap { f(_).forSize(size) }
      }

    SGen(forSize2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))
}
