package fpinscala.iomonad

import language.postfixOps
import language.higherKinds
import scala.io.StdIn.readLine

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
}
