package fpinscala.streamingio

import language.implicitConversions
import language.higherKinds
import language.postfixOps
import fpinscala.iomonad.IO1.IO

object ImperativeAndLazyIO {

  // List 15-1
  def linesGt5(filename: String): IO[Boolean] = IO {
    val src = scala.io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines
      while (count <= 5 && lines.hasNext) {
        lines.next // 次の要素に進むという副作用がある。
        count += 1
      }
      count > 5
    } finally src.close
  }

  // List 15-2
  // リソースの開放がストリームの最後まで走査した場合だけ。
  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream.append {
      src.close
      Stream.empty
    }
  }
}

object SimpleStreamTransducers {

  // List 15-3
  sealed trait Process[I, O] {
    import Process._

    // List 15-4
    def apply(s: Stream[I]): Stream[O] =
      this match {
        case Halt() => Stream()
        case Await(recv) =>
          s match {
            case h #:: t => recv(Some(h))(t)
            case xs => recv(None)(xs)
          }
        case Emit(h, t) => h #:: t(s)
      }

    // List 15-6
    def repeat: Process[I, O] = {

      def go(p: Process[I, O]): Process[I, O] =
        p match {
          case Halt() => go(this)
          case Await(recv) => Await {
            case None => recv(None)
            case i => go(recv(i))
          }
          case Emit(h, t) => Emit(h, go(t))
        }

      go(this)
    }

    // EXERCISE 15.5
    def |>[O2](p2: Process[O, O2]): Process[I, O2] =
      p2 match {
        case Halt() => Halt()
        case Emit(h, t) => Emit(h, this |> t)
        case Await(f) =>
          this match {
            case Emit(h, t) => t |> f(Some(h))
            case Halt() => Halt() |> f(None)
            case Await(g) => Await((i: Option[I]) => g(i) |> p2)
          }
      }

    // List 15-10
    def ++(p: => Process[I, O]): Process[I, O] =
      this match {
        case Halt() => p
        case Emit(h, t) => Emit(h, t ++ p)
        case Await(recv) => Await(recv andThen (_ ++ p))
      }

    // List 15-11
    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] =
      this match {
        case Halt() => Halt()
        case Emit(h, t) => f(h) ++ t.flatMap(f)
        case Await(recv) => Await(recv andThen (_ flatMap f))
      }
  }

  object Process {

    // List 15-3
    // 放出する。
    // head値を出力ストリームに書き出さなければならないことをドライバに合図します。
    // それにより、状態機械がtail状態へ遷移します。
    case class Emit[I, O](
        head: O,
        tail: Process[I, O] = Halt[I, O]()
    ) extends Process[I, O]

    // 待つ。
    // 入力ストリームの値をリクエストします。ドライバは次に利用可能な値をrecv関数に渡し、
    // それ以上要素がない場合はNoneを返します。
    case class Await[I, O](
        recv: Option[I] => Process[I, O]
    ) extends Process[I, O]

    // 停止する。
    // それ以上入力を読み取らない、または出力に書き出さないことをドライバに合図します。
    case class Halt[I, O]() extends Process[I, O]

    // List 15-8
    // Emitを生成する。
    def emit[I, O](
      head: O,
      tail: Process[I, O] = Halt[I, O]
    ): Process[I, O] =
      Emit(head, tail)

    // EXERCISE 15.1
    def await[I, O](
      f: I => Process[I, O],
      fallback: Process[I, O] = Halt[I, O]()
    ): Process[I, O] =
      Await[I, O] {
        case Some(i) => f(i)
        case None => fallback
      }
    // Await {
    // recv =>
    // recv match {
    // case Some(i) => f(i)
    // case None => fallback
    // }
    // }

    // List 15-5
    // 任意の関数 f: I => O を Process[I, O] に変換する。
    def liftOne[I, O](f: I => O): Process[I, O] =
      Await {
        case Some(i) => Emit(f(i))
        case None => Halt()
      }

    // List 15-7
    def lift[I, O](f: I => O): Process[I, O] =
      liftOne(f).repeat

    // List 15-8
    def filter[I](p: I => Boolean): Process[I, I] =
      Await[I, I] {
        case Some(i) if p(i) => emit(i)
        case _ => Halt()
      }.repeat

    // これまでに検出された値の累積合計を出力する。
    def sum: Process[Double, Double] = {

      def go(acc: Double): Process[Double, Double] =
        // 関数を受け取るメソッドに対し、その関数の引数でmatchしたい場合、「引数 match」を省略できる。
        Await {
          case Some(d) => Emit(d + acc, go(d + acc))
          case None => Halt()
        }

      go(0.0)
    }

    // EXERCISE 15.1
    def take[I](n: Int): Process[I, I] =
      if (n < 0) Halt()
      else await(i => emit(i, take[I](n - 1)))

    // EXERCISE 15.1
    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) id // 残りの出力は無限。
      // if (n <= 0) lift(I => I)
      else await(i => drop[I](n - 1))

    // EXERCISE 15.1
    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await(i =>
        if (f(i)) emit(i, takeWhile(f))
        else Halt())

    // EXERCISE 15.1
    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await(i =>
        if (f(i)) dropWhile(f)
        else emit(i, id)) // 残りの出力は無限。

    def id[I]: Process[I, I] = lift(identity)
    // def id[I]: Process[I, I] = lift(I => I)

    // EXERCISE 15.2
    def count[I]: Process[I, Int] = {

      def go(n: Int): Process[I, Int] =
        await((i: I) => emit(n + 1, go(n + 1)))

      go(0)
    }

    // EXERCISE 15.3
    def mean: Process[Double, Double] = {

      def go(sum: Double, count: Double): Process[Double, Double] =
        await((d: Double) => emit((sum + d) / (count + 1), go(sum + d, count + 1)))

      go(0.0, 0.0)
    }

    // List 15-9
    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) => f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      })

    // EXERCISE 15.4
    def sumViaLoop: Process[Double, Double] =
      loop(0.0)((d: Double, acc) => (acc + d, acc + d))

    // EXERCISE 15.4
    def countViaLoop[I]: Process[I, Int] =
      loop(0)((_: I, n) => (n + 1, n + 1))

    // List 15-2
    import fpinscala.monads.Monad
    def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
      new Monad[({ type f[x] = Process[I, x] })#f] {

        def unit[O](o: => O): Process[I, O] = emit(o)

        def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] =
          p flatMap f
      }
  }
}
