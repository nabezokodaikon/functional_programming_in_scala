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
}
