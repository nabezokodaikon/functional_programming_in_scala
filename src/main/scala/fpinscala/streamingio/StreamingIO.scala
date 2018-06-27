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
  sealed trait Process[I, O]

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
}
