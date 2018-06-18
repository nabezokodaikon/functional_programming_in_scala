package fpinscala.iomonad

import language.postfixOps
import language.higherKinds
import scala.io.StdIn.readLine
import fpinscala.monads._

case class Player(name: String, score: Int)

object Player {
  import IO0._

  // List 13-1
  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!")
    else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw.")

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None

  def contest2(p1: Player, p2: Player): Unit =
    winner(p1, p2) match {
      case Some(Player(name, _)) => println(s"${name} is the winner!")
      case None => println("It's a draw.")
    }

  def winnerMsg(p: Option[Player]): String =
    p map {
      case Player(name, _) => s"${name} is the winner!"
    } getOrElse ("It's a draw.")

  def contest3(p1: Player, p2: Player): Unit =
    println(winnerMsg(winner(p1, p2)))

  def contest4(p1: Player, p2: Player): IO =
    PrintLine(winnerMsg(winner(p1, p2)))
}

object IO0 {

  // List 13-2
  trait IO { self => // 引数selfにより、このオブジェクトをthisではなくselfとして参照できる。

    def run: Unit

    def ++(io: IO): IO = new IO {
      def run = {
        self.run // selfは外側のIOを参照する。
        io.run
      }
    }
  }

  // List 13-2
  object IO {

    def empty: IO = new IO {
      def run = ()
    }
  }

  def PrintLine(msg: String): IO =
    new IO {
      def run = println(msg)
    }

  // List 13-3
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  /*
  def converter2: IO = {
    val prompt: IO = PrintLine("Enter a temperature in degrees Fahrenheit: ")
    // now what ???
  }
  */
}

object IO1 {

  // List 13-4
  sealed trait IO[A] { self =>

    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] {
        def run = f(self.run)
      }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] {
        def run = f(self.run).run
      }

    def map2[B, C](ib: IO[B])(f: (A, B) => C): IO[C] =
      self.flatMap(a => ib.map(b => f(a, b)))

    def **[B](ib: IO[B]): IO[(A, B)] =
      self.map2(ib)((_, _))
  }

  // List 13-5
  object IO extends Monad[IO] {

    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

    // このメソッドにより、IO { ... } のように、
    // IOブロックを生成するための関数適用構文を使用できるようになる。
    def apply[A](a: => A): IO[A] = unit(a)
  }

  // List 13-6
  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  val echo = ReadLine.flatMap(PrintLine)
  val readInt = ReadLine.map(_.toInt)
  val readInts = readInt ** readInt
  val replicateM = IO.replicateM(10, ReadLine)

  // List 13-7
  // def factorial(n: Int): IO[Int] = // ミュータブルなIO参照を使った命令型のfactorial。
  // for {
  // acc <- ref(1) // ミュータブルな参照の割り当て。
  // _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip) // ループ内で参照を変更。
  // result <- acc.get // 参照先の値を取得するための間接参照。
  // } yield result

  // val factorialREPL: IO[Unit] = sequence_(
  // IO { println(helpstring) },
  // doWhile { IO { readLine } } {
  // line =>
  // val ok = line != "q"
  // when(ok) {
  // for {
  // n <- factorial(line.toInt)
  // _ <- IO { println("factorial: " + n) }
  // } yield ()
  // }
  // }
  // )

  // condがtrueを返す限り、1つ目の引数の作用を繰り返す。
  // def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] =
  // for {
  // a1 <- a
  // ok <- cond(a1)
  // } yield ()

  // 引数の作用を無限に繰り返す。
  // def forever[A, B](a: F[A]): F[B] = {
  // lazy val t: F[B] = forever(a)
  // a flatMap (_ => t)
  // }

  // ストリームを関数fで畳み込み、作用を結合し、その結果を返す。
  // def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] =
  // l match {
  // case h #:: t => f(z, h) flatMap(z2 => foldM(t)(z2)(f))
  // }

  // foldM関数と同じだが、結果を無視する。
  // def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] =
  // skip { foldM(1)(z)(f) }

  // ストリームの要素ごとにf関数を呼び出し、作用を結合する。
  // def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
  // foldM_(1)(())((u, a) => skip(f(a)))

}

object IO2a {

  // List 13-8
  // 余分なステップを実行せずに直ちにAを返す純粋な計算。
  // このコンストラクタを検出した時点で、runは計算が終了していることを認識する。
  case class Return[A](a: A) extends IO[A]

  // 計算の一時的な中断。
  // resumeは引数を受け取らない関数だが、何らかの作用を持ち、結果を返す。
  case class Suspend[A](resume: () => A) extends IO[A]

  // 2つのステップの合成。
  // flatMapを関数ではなくデータコンストラクタとして具体化する。
  // これを検出した時点で、runは部分計算subを処理し、subが結果を生成したところでkを継続する必要がある。
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  sealed trait IO[A] {

    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)

    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  // List 13-10
  @annotation.tailrec
  def run[A](io: IO[A]): A =
    io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>
        x match {
          case Return(a) => run(f(a))
          case Suspend(r) => run(f(r))
          case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
        }
    }
}

object Main extends App {

  // List 13-6
  // IO1.converter.run

  // IO1.echo.run
  // IO1.readInt.run
  // IO1.readInts.run
  IO1.replicateM.run
}
