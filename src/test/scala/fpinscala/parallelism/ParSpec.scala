package fpinscala.parallelism

import java.util.concurrent._
import org.scalatest.FunSuite

class ParSpec extends FunSuite {

  test("EXERCISE 7.4") {
    val es = Executors.newFixedThreadPool(1)
    val par = Par.asyncF((a: Int) => a * 2)
    var r = Par.run(es)(par(3))
    assert(r.get == 6)
  }

  test("sortPar") {
    val es = Executors.newFixedThreadPool(1)
    val par = Par.sortPar(Par.unit(List(2, 1, 6, 4, 5, 3)))
    var f = par(es)
    val r = f.get
    assert(r == List(1, 2, 3, 4, 5, 6))
  }

  test("EXERCISE 7.5 parMap via sequence") {
    val es = Executors.newFixedThreadPool(2)
    val l = List(1, 2, 3)
    val pl = Par.parMap(l)(a => a * 2)
    val f = pl(es)
    val r = f.get
    assert(r == List(2, 4, 6))
  }
}
