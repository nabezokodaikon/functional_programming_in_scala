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
