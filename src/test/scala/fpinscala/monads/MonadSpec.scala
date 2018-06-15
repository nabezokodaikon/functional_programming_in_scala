package fpinscala
package monads

import org.scalatest.FunSuite
import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds

class MonadSpec extends FunSuite {

  test("List 11-2") {
    val f = Functor.listFunctor
    assert(f.map(List(1, 2, 3))(a => a * 2) == List(2, 4, 6))
  }

  test("List 11-3 distribute(unzip)") {
    val f = Functor.listFunctor
    val l = List((1, 2), (3, 4), (5, 6))
    assert(f.distribute(l) == (List(1, 3, 5), List(2, 4, 6)))
  }
}
