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

  test("List 11-4 condistribute") {
    import Functor._
    val a = listFunctor.condistribute(Left(List(1, 2, 3)))
    assert(a == List(Left(1), Left(2), Left(3)))
    val b = listFunctor.condistribute(Right(List(1, 2, 3)))
    assert(b == List(Right(1), Right(2), Right(3)))
  }

  test("List 11-7 Mon trait") {
    import Mon._
    val a = optionMon.map2(Some(1), Some(2))((a, b) => a + b)
    assert(a == Some(3))
  }
}
