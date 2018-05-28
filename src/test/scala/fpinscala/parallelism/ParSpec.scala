package fpinscala.parallelism

import org.scalatest.FunSuite

class ParSpec extends FunSuite {

  test("List 7-1 sum") {
    import Par.sum
    assert(sum(IndexedSeq[Int]()) == 0)
    assert(sum(IndexedSeq(1)) == 1)
    assert(sum(IndexedSeq(1, 2, 3, 4)) == 10)
  }

  test("List 7-2 sum_2") {
    import Par.sum_2
    assert(sum_2(IndexedSeq[Int]()) == 0)
    assert(sum_2(IndexedSeq(1)) == 1)
    assert(sum_2(IndexedSeq(1, 2, 3, 4)) == 10)
  }

  test("EXERCISE 7.1 map2") {
    import Par.map2
    val a = Par(1)
    val b = Par(2)
    assert(map2(a, b)((a, b) => a + b) == Par(3))
  }
}
