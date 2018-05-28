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
}
