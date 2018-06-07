package fpinscala.monoids

import fpinscala.state.RNG.SimpleRNG
import fpinscala.testing.{ Gen, Prop }
import org.scalatest.FunSuite

class MonoidSpec extends FunSuite {

  test("List 10-2 stringMonoid") {
    import Monoid.stringMonoid.op
    val a = op(op("a", "b"), "c")
    val b = op("a", op("b", "c"))
    assert(a == b)
  }

  test("List 10-3 listMonoid") {
    import Monoid.listMonoid
    val a = listMonoid.op(listMonoid.op(List(1, 2), List(3, 4)), List(5, 6))
    val b = listMonoid.op((List(1, 2)), listMonoid.op(List(3, 4), List(5, 6)))
    assert(a == b)
  }

  test("EXERCISE 10.1 intAddition") {
    import Monoid.intAddition.op
    val a = op(op(1, 2), 3)
    val b = op(1, op(2, 3))
    assert(a == b)
  }

  test("EXERCISE 10.1 intMultiplication") {
    import Monoid.intMultiplication.op
    val a = op(op(1, 2), 3)
    val b = op(1, op(2, 3))
    assert(a == b)
  }

  test("EXERCISE 10.1 booleanOr") {
    import Monoid.booleanOr.op
    val a = op(op(true, true), true)
    val b = op(true, op(true, true))
    val c = op(op(false, false), false)
    val d = op(false, op(false, false))
    val e = op(op(false, true), false)
    val f = op(false, op(true, false))
    val g = op(op(false, true), true)
    val h = op(true, op(true, false))
    assert(a == b == e == f == g == h)
    assert(c == d)
    assert(a != c)
  }

  test("EXERCISE 10.1 booleanAnd") {
    import Monoid.booleanOr.op
    val a = op(op(true, true), true)
    val b = op(true, op(true, true))
    val c = op(op(false, false), false)
    val d = op(false, op(false, false))
    val e = op(op(false, true), false)
    val f = op(false, op(true, false))
    val g = op(op(false, true), true)
    val h = op(true, op(true, false))
    assert(a == b)
    assert(c == d == e == f == g == h)
    assert(a != c)
  }

  test("EXERCISE 10.2 firstOptionMonoid") {
    import Monoid.firstOptionMonoid
    val a = firstOptionMonoid.op(firstOptionMonoid.op(Some(1), Some(2)), Some(3))
    assert(a.isInstanceOf[Some[Int]] == true)
    val b = firstOptionMonoid.op(firstOptionMonoid.op(Some(1), None), Some(3))
    assert(b.isInstanceOf[Some[Int]] == true)
    val c = firstOptionMonoid.op(firstOptionMonoid.op(Some(1), None), None)
    assert(c.isInstanceOf[Some[Int]] == true)
    val d = firstOptionMonoid.op(firstOptionMonoid.op(None, None), None)
    assert(d == None)
  }

  test("exercise 10.2 lastOptionMonoid") {
    import Monoid.lastOptionMonoid
    val a = lastOptionMonoid.op(lastOptionMonoid.op(Some(1), Some(2)), Some(3))
    assert(a.isInstanceOf[Some[Int]] == true)
    val b = lastOptionMonoid.op(lastOptionMonoid.op(Some(1), None), Some(3))
    assert(b.isInstanceOf[Some[Int]] == true)
    val c = lastOptionMonoid.op(lastOptionMonoid.op(Some(1), None), None)
    assert(c.isInstanceOf[Some[Int]] == true)
    val d = lastOptionMonoid.op(lastOptionMonoid.op(None, None), None)
    assert(d == None)
  }

  test("EXERCISE 10.3 endMonoid") {
    import Monoid.endoMonoid
    val a = endoMonoid.op(endoMonoid.op(((x: Int) => x + 1), ((x: Int) => x + 2)), ((x: Int) => x + 3))
    assert(a(2) == 8)
  }

  test("EXERCISE 10.4 monoidLaws") {
    import fpinscala.monoids.Monoid._
    {
      val gen = Gen.unit("a")
      val p = monoidLaws(stringMonoid, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(Nil)
      val p = monoidLaws(listMonoid, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(1)
      val p = monoidLaws(intAddition, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(1)
      val p = monoidLaws(intMultiplication, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(false)
      val p = monoidLaws(booleanOr, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(false)
      val p = monoidLaws(booleanAnd, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(None)
      val p = monoidLaws(firstOptionMonoid, gen)
      Prop.run(p)
    }

    {
      val gen = Gen.unit(None)
      val p = monoidLaws(lastOptionMonoid, gen)
      Prop.run(p)
    }
  }

  test("List 10-4 concatenate") {
    import Monoid.concatenate
    val m = new Monoid[Int] {
      def op(a: Int, b: Int) = a + b
      val zero = 0
    }
    val res = concatenate(List(3, 4), m)
    assert(res == 7)
  }

  test("EXERCISE 10.5 foldMap") {
    import Monoid.foldMap
    val m = new Monoid[Int] {
      def op(a: Int, b: Int) = a + b
      val zero = 2
    }
    val res = foldMap(List("3", "4"), m)(a => a.toInt)
    assert(res == 9)
  }

  test("EXERCISE 10.6 foldRight") {
    import Monoid.foldRight
    val res = foldRight(List(1, 2, 3))(0)((a, b) => a + b)
    assert(res == 6)
  }

  test("EXERCISE 10.6 foldLeft") {
    import Monoid.foldLeft
    val res = foldLeft(List(1, 2, 3))(0)((a, b) => a + b)
    assert(res == 6)
  }

  test("EXERCISE 10.9 foldMapV") {
    import Monoid.foldMapV
    val m = new Monoid[Int] {
      def op(a: Int, b: Int) = a + b
      val zero = 0
    }
    val res = foldMapV(IndexedSeq("3", "4"), m)(a => a.toInt)
    assert(res == 7)
  }

  test("EXERCISE 10.9 orderd") {
    import Monoid.ordered
    assert(ordered(IndexedSeq(3, 2, 1)) == false)
    assert(ordered(IndexedSeq(1, 3, 5)) == true)
  }

  test("EXERCISE 10.10 wcMonoid") {
    import Monoid._
    import wcMonoid._

    assert(op(Stub("Mon Tue"), Stub("Wed Thu Fri")) == Stub("Mon TueWed Thu Fri"))
    assert(op(Stub("Mon Tue"), Part("Wed", 1, "Fri")) == Part("Mon TueWed", 1, "Fri"))
    assert(op(Part("Mon Tue", 1, "Wed"), Stub("Thu Fri")) == Part("Mon Tue", 1, "WedThu Fri"))
    assert(op(Part("Mon Tue", 1, "Wed"), Part("Thu", 0, "Fri")) == Part("Mon Tue", 2, "Fri"))
    assert(op(Part("Mon Tue Wed Thu Fri", 3, ""), Part("", 0, "")) == Part("Mon Tue Wed Thu Fri", 3, ""))
  }

  test("EXERCISE 10.11 count") {
    import Monoid.count
    assert(count("") == 0)
    assert(count("Mon") == 1)
    assert(count("Mon Tue") == 2)
    assert(count("Mon Tue Wed Thu Fri") == 5)
    assert(count(" ") == 0)
    assert(count(" Mon ") == 1)
    assert(count(" Mon Tue ") == 2)
    assert(count(" Mon Tue Wed Thu Fri ") == 5)
  }

  test("EXERCISE 10.12 ListFoldable") {
    import Monoid._
    assert(ListFoldable.foldRight(List(1, 2))(0)((b, a) => b + a) == 3)
    assert(ListFoldable.foldLeft(List(1, 2))(0)((a, b) => a + b) == 3)
    assert(ListFoldable.concatenate(List(1, 2))(intAddition) == 3)
  }

  test("EXERCISE 10.12 IndexedSeqFoldable") {
    import Monoid._
    assert(IndexedSeqFoldable.foldRight(IndexedSeq(1, 2))(0)((b, a) => b + a) == 3)
    assert(IndexedSeqFoldable.foldLeft(IndexedSeq(1, 2))(0)((a, b) => a + b) == 3)
    assert(IndexedSeqFoldable.concatenate(IndexedSeq(1, 2))(intAddition) == 3)
  }

  test("EXERCISE 10.12 StreamFoldable") {
    import Monoid._
    assert(StreamFoldable.foldRight(Stream(1, 2))(0)((b, a) => b + a) == 3)
    assert(StreamFoldable.foldLeft(Stream(1, 2))(0)((a, b) => a + b) == 3)
  }

  test("EXERCISE 10.15 toList") {
    import Monoid._
    assert(TreeFoldable.toList(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == List(1, 2, 3))
  }

  test("10.6.2 合成されたモノイドを使った走査") {
    import Monoid._
    // 平均を計算するためにリストの長さと合計を同時に取得する。
    val m = productMonoid(intAddition, intAddition)
    val (length, sum) = ListFoldable.foldMap(List(1, 2, 3, 4))(a => (1, a))(m)
    assert(length == 4)
    assert(sum == 10)
  }
}
