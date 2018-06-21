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
}
