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
}
