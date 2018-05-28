package fpinscala.parallelism

import org.scalatest.FunSuite

class ParSpec extends FunSuite {

  test("List 7-1 sum") {
    import Par.sum
    assert(sum(IndexedSeq[Int]()) == 0)
    assert(sum(IndexedSeq(1)) == 1)
    assert(sum(IndexedSeq(1, 2, 3, 4)) == 10)
  }
}
