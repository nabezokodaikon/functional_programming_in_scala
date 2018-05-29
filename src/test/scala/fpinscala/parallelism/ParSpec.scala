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

  test("EXERCISE 7.6 parFilter") {
    val es = Executors.newFixedThreadPool(2)
    val l = List(1, 2, 3)
    val pl = Par.parFilter(l)(a => a % 2 == 1)
    val f = pl(es)
    val r = f.get
    assert(r == List(1, 3))
  }

  test("List 7-15 choice") {
    val es = Executors.newFixedThreadPool(2)

    val tb = Par.unit(true)
    val tl = List(1, 2, 3)
    val tpl = Par.parFilter(tl)(a => a % 2 == 1)

    val fb = Par.unit(false)
    val fl = List(1, 2, 3)
    val fpl = Par.parMap(fl)(a => a * 2)

    assert(Par.equal(es)(Par.choice(tb)(tpl, fpl), Par.unit(List(1, 3))) == true)
    assert(Par.equal(es)(Par.choice(fb)(tpl, fpl), Par.unit(List(2, 4, 6))) == true)
  }

  test("exercise 7.11 choiceN") {
    val es = Executors.newFixedThreadPool(2)

    val al = List(1, 2, 3)
    val apl = Par.parFilter(al)(a => a % 2 == 1)

    val bl = List(1, 2, 3)
    val bpl = Par.parMap(bl)(a => a + 2)

    val cl = List(1, 2, 3)
    val cpl = Par.parMap(cl)(a => a * 2)

    val l = List(apl, bpl, cpl)

    assert(Par.equal(es)(Par.choiceN(Par.unit(0))(l), Par.unit(List(1, 3))) == true)
    assert(Par.equal(es)(Par.choiceN(Par.unit(1))(l), Par.unit(List(3, 4, 5))) == true)
    assert(Par.equal(es)(Par.choiceN(Par.unit(2))(l), Par.unit(List(2, 4, 6))) == true)
  }

  test("EXERCISE 7.12 choiceMap") {
    val es = Executors.newFixedThreadPool(2)
    val key = Par.unit("a")
    val map = Map("a" -> Par.unit(1))
    assert(Par.equal(es)(Par.choiceMap(key)(map), Par.unit(1)) == true)
  }

  test("EXERCISE 7.13 choiceViaChooser") {
    val es = Executors.newFixedThreadPool(2)

    val tb = Par.unit(true)
    val tl = List(1, 2, 3)
    val tpl = Par.parFilter(tl)(a => a % 2 == 1)

    val fb = Par.unit(false)
    val fl = List(1, 2, 3)
    val fpl = Par.parMap(fl)(a => a * 2)

    assert(Par.equal(es)(Par.choiceViaChooser(tb)(tpl, fpl), Par.unit(List(1, 3))) == true)
    assert(Par.equal(es)(Par.choiceViaChooser(fb)(tpl, fpl), Par.unit(List(2, 4, 6))) == true)
  }

  test("EXERCISE 7.13 choiceNViaChooser") {
    val es = Executors.newFixedThreadPool(2)

    val al = List(1, 2, 3)
    val apl = Par.parFilter(al)(a => a % 2 == 1)

    val bl = List(1, 2, 3)
    val bpl = Par.parMap(bl)(a => a + 2)

    val cl = List(1, 2, 3)
    val cpl = Par.parMap(cl)(a => a * 2)

    val l = List(apl, bpl, cpl)

    assert(Par.equal(es)(Par.choiceNViaChooser(Par.unit(0))(l), Par.unit(List(1, 3))) == true)
    assert(Par.equal(es)(Par.choiceNViaChooser(Par.unit(1))(l), Par.unit(List(3, 4, 5))) == true)
    assert(Par.equal(es)(Par.choiceNViaChooser(Par.unit(2))(l), Par.unit(List(2, 4, 6))) == true)
  }

  test("EXERCISE 7.14 flatMapViaJoin") {
    val es = Executors.newFixedThreadPool(2)
    val pa = Par.unit(1)
    val pb = Par.flatMapViaJoin(pa)(a => Par.unit(a.toString))
    assert(Par.equal(es)(pb, Par.unit("1")) == true)
  }

  test("EXERCISE 7.14 joinViaFlatMap") {
    val es = Executors.newFixedThreadPool(2)
    val a = Par.unit(1)
    val b = Par.unit(a)
    assert(Par.equal(es)(Par.joinViaFlatMap(b), a) == true)
  }
}
