package fpinscala.iomonad

import org.scalatest.FunSuite

class IOSpec extends FunSuite {

  test("List 13-1") {
    Player.contest(Player("taro", 100), Player("jiro", 90))
    Player.contest(Player("taro", 50), Player("jiro", 100))
    Player.contest(Player("taro", 50), Player("jiro", 50))

    Player.contest2(Player("taro", 100), Player("jiro", 90))
    Player.contest2(Player("taro", 50), Player("jiro", 100))
    Player.contest2(Player("taro", 50), Player("jiro", 50))

    Player.contest3(Player("taro", 100), Player("jiro", 90))
    Player.contest3(Player("taro", 50), Player("jiro", 100))
    Player.contest3(Player("taro", 50), Player("jiro", 50))

    Player.contest4(Player("taro", 50), Player("jiro", 50)).run
  }

  test("List 13-2") {
    import IO0._

    val io1 = new IO {
      def run = println("IO 1")
    }

    val io2 = new IO {
      def run = println("IO 2")
    }

    val io3 = io1 ++ io2
    io3.run
  }

  test("List 13-10") {
    import IO2a._

    {
      val io = Return(1)
      assert(IO2a.run(io) == 1)
    }

    {
      val io = Suspend(() => 1)
      assert(IO2a.run(io) == 1)
    }

    {
      val io = Return(1)
      val f = FlatMap(io, (a: Int) => Return(a * 2))
      assert(IO2a.run(io) == 1)
    }

    {
      val io = Suspend(() => 1)
      val f = FlatMap(io, (a: Int) => Return(a * 2))
      assert(IO2a.run(io) == 1)
    }

    {
      val io = FlatMap(Return(1), (a: Int) => Return(a * 2))
      val f = FlatMap(io, (a: Int) => Return(a * 3))
      assert(IO2a.run(io) == 2)
    }
  }
}
