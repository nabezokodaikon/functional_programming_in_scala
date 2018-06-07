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

  test("List 11-2 listFunctor") {
    import Functor._
    assert(listFunctor.map(List(1, 2, 3))(_.toString) == List("1", "2", "3"))
  }

  test("List 11-3 distribute") {
    import Functor._
    assert(listFunctor.distribute(List((1, "a"), (2, "b"), (3, "c"))) == (List(1, 2, 3), List("a", "b", "c")))
  }
}
