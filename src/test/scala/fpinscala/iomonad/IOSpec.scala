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

    {
      val io = FlatMap(Suspend(() => 1), (a: Int) => Return(a * 2))
      val f = FlatMap(io, (a: Int) => Return(a * 3))
      assert(IO2a.run(io) == 2)
    }

    {
      val f: Int => IO[Int] = (x: Int) => Return(x)
      // f: Int => IO[Int] = <function1>
      val g = List.fill(100000)(f).foldLeft(f) { (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b) }
      }
      // g: Int => IO[Int] = <function1>

      assert(IO2a.run(g(0)) == 0)
      assert(IO2a.run(g(42)) == 42)
      assert(IO2a.run(g(100000)) == 100000)
    }
  }

  test("List 13-13") {
    import java.util.concurrent._
    import fpinscala.parallelism._
    import fpinscala.parallelism.Par._
    import IO2c._

    {
      val es = Executors.newFixedThreadPool(1)
      val a = Return(1)
      val p = IO2c.run(a)
      val f = Par.run(es)(p)
      assert(f == UnitFuture(1))
    }

    {
      val es = Executors.newFixedThreadPool(1)
      val par = unit(1)
      val async = Suspend(par)
      val a = IO2c.run(async)
      val f = Par.run(es)(a)
      assert(f == UnitFuture(1))
    }
  }

  test("EXERCISE 13.2") {
    import IO3._
    val r = Return[Function0, Int](10)
    val s = Suspend[Function0, String](() => "abc")
    val f = FlatMap[Function0, Int, String](r, a => Return(a.toString))

    assert(runTrampoline(r) == 10)
    assert(runTrampoline(s) == "abc")
    assert(runTrampoline(f) == "10")
  }
}
