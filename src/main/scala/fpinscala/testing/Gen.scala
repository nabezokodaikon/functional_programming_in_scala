package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.{ RNG, State }
import fpinscala.state.RNG.SimpleRNG
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

object Main {

  def testRunner() = {
    val rng = SimpleRNG(1)

    {
      println("pair")
      val s = Gen.pair(1, 10)
      val a = s.sample.run(rng)
      println(a)
    }

    {
      println("With Option")
      val s1 = Gen.unit(5).map(a => Some(a))
      println(s1.sample.run(rng))

      val s2 = Gen.unit(Some(5)).map { s => s match { case Some(a) => a } }
      println(s2.sample.run(rng))
    }

    {
      println("Chapter 8.4.1 with EXERCISE 8.13")
      val smallInt = Gen.choose(-10, 10)
      val maxProp = forAll(Gen.listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }
      Prop.run(maxProp)
    }

    {
      println("EXERCISE 8.14 sortedProp")
      val smallInt = Gen.choose(-10, 10)
      val sortedProp = forAll(Gen.listOf(smallInt)) {
        l =>
          val ls = l.sorted
          l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
      }
      Prop.run(sortedProp)
    }
  }

  def main(args: Array[String]): Unit = {
    // testRunner()
  }
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // EXERCISE 8.9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
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

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
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

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

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

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after ${n} passed tests:\n ${msg}")
      case Passed =>
        println(s"+ OK, passed ${testCases} test.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
  }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  // def check(p: => Boolean): Prop = {
  // lazy val result = p
  // forAll(Gen.unit(()))(_ => result)
  // }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }
  // val p2 = checkPar {
  // equal(
  // Par.map(Par.unit(1))(_ + 1),
  // Par.unit(2)
  // )
  // }

  // List 8-9
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  // List 8-10
  val S = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    Gen.unit(Executors.newCachedThreadPool) -> 0.25
  )

  // List 8-12
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def checkPar(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
    // forAllPar(Gen.unit(()))(_ => p)
  }

  // List 8-15
  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  // EXERCISE 8.17
  val forkProp = Prop.forAllPar(Gen.pint2)(i => equal(Par.fork(i), i)) tag "fork"
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

  // List 8-11
  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}

object Gen {

  /*
   * EXERCISE 8.4
   * startからstopExclusiveno範囲内の整数を生成する。
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegariveInt).map(n => start + n % (stopExclusive - start)))

  /*
   * 常にaの値を生成する。
   */
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  /*
   * ジェネレータgを使って長さnのリストを生成する。
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  /*
   * Doubleの乱数を生成。
   */
  val uniform: Gen[Double] = Gen(State(RNG.double))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  // EXERCISE 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  /*
   * 指定された長さのランダムな文字列を生成。
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  def string: SGen[String] = SGen(stringN)

  def pair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    for {
      a <- choose(start, stopExclusive)
      b <- choose(start, stopExclusive)
    } yield (a, b)

  // List 8-14
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  // EXERCISE 8.16
  val pint2: Gen[Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))

  // List 8-16
  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => (s => i))
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
