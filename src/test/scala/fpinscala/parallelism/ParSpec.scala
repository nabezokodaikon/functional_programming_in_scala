package fpinscala.parallelism

import java.util.concurrent._
import org.scalatest.FunSuite

class ParSpec extends FunSuite {

  /*
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

  test("sum_4") {
    import Par.sum_4
    assert(sum_4(IndexedSeq[Int]()) == Par(0))
    assert(sum_4(IndexedSeq(1)) == Par(1))
    assert(sum_4(IndexedSeq(1, 2, 3, 4)) == Par(10))
  }
  */

  test("EXERCISE 7.4") {
    val es = Executors.newFixedThreadPool(1)
    val par = Par.asyncF((a: Int) => a * 2)
    var r = Par.run(es)(par(3))
    assert(r.get == 6)
  }
}
