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

  test("List 11-4 codistribute") {
    val f = Functor.listFunctor
    val l = Left(List(1, 2))
    assert(f.codistribute(l) == List(Left(1), Left(2)))
  }

  test("EXERCISE 11.1 listMonad") {
    val m = Monad.listMonad
    assert(m.unit(1) == List(1))
    assert(m.flatMap(List(1, 2, 3))(a => List(a * 2)) == List(2, 4, 6))
  }

  test("EXERCISE 11.1 optionMonad") {
    val m = Monad.optionMonad
    assert(m.unit(1) == Some(1))
    assert(m.flatMap(Some(1))(a => Some(a * 2)) == Some(2))
    assert(m.flatMap(None)((a: Int) => Some(a * 2)) == None)
  }

  test("EXERCISE 11.1 streamMonad") {
    val m = Monad.streamMonad
    assert(m.unit(1) == Stream(1))
    assert(m.flatMap(Stream(1, 2, 3))(a => Stream(a * 2)) == Stream(2, 4, 6))
  }
}
