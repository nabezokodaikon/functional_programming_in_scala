package fpinscala.localeffects

import org.scalatest.FunSuite

class LocalEffectsSpec extends FunSuite {

  test("List 14-1 quicksort") {
    import Mutable._
    assert(quicksort(List[Int]()) == List[Int]())
    assert(quicksort(List(1)) == List(1))
    assert(quicksort(List(3, 2, 1)) == List(1, 2, 3))
  }

  test("List 14-2") {
    import Mutable._
    val st = ST(10)
    println(st.map(a => a.toString))
    println(st.flatMap(a => ST(a.toString)))
  }

  test("List 14-3 まだ実行できない。") {
    import Mutable._
    for {
      r1 <- STRef[Nothing, Int](1)
      r2 <- STRef[Nothing, Int](2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 2)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  test("List 14-5") {
    import Mutable._
    val p = new RunnableST[(Int, Int)] {
      def apply[S] = for {
        r1 <- STRef(1)
        r2 <- STRef(10)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 2)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    val r = ST.runST(p)
    assert(r == (11, 3))
  }

  test("EXERCISE 14.1") {
    import Mutable._
    val st = STArray(3, "a")
    val map = Map(0 -> "a", 1 -> "b", 2 -> "c")
    val p = new RunnableST[List[String]] {
      def apply[S] = for {
        a <- STArray(3, "a")
        b <- a.fill(map)
        l <- a.freeze
      } yield l
    }

    val r = ST.runST(p)
    assert(r == List("a", "b", "c"))
  }

  test("List 14-7") {
    import Mutable._
    val p = new RunnableST[List[Int]] {
      def apply[S] = for {
        a <- STArray.fromList(List(1, 2, 3))
        _ <- a.write(1, 5)
        b <- a.freeze
      } yield b
    }

    val list = ST.runST(p)
    assert(list == List(1, 5, 3))
  }

  test("List 14-9 quicksort") {
    val l = Immutable.quicksort(List(5, 3, 1, 4, 2))
    assert(l == List(1, 2, 3, 4, 5))
  }
}
