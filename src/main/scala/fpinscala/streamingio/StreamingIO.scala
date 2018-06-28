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
      // Await {
      // recv =>
      // recv match {
      // case Some(d) => Emit(d + acc, go(d + acc))
      // case None => Halt()
      // }
      // }

      go(0.0)
    }
  }
}
