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

  test("EXERCISE 11.1 Option Monad") {
    import Monad._
    val a = optionMonad.map2(Some(2), Some(3))((a, b) => a * b)
    assert(a == Some(6))
  }

  test("EXERCISE 11.1 List Monad") {
    import Monad._
    val a = listMonad.map2(List(1, 2, 3), List(4, 5, 6))((a, b) => a + b)
    assert(a == List(5, 6, 7, 6, 7, 8, 7, 8, 9))
  }

  test("EXERCISE 11.1 Stream Monad") {
    import Monad._
    val a = streamMonad.map2(Stream(1, 2, 3), Stream(4, 5, 6))((a, b) => a + b)
    assert(a.toList == List(5, 6, 7, 6, 7, 8, 7, 8, 9))
    val b = streamMonad.map(Stream(1, 2, 3))(a => a * 2)
    assert(b.toList == List(2, 4, 6))
  }

  test("EXERCISE 11.3 sequence") {
    import Monad._
    assert(optionMonad.sequence(List(None)) == None)
    assert(optionMonad.sequence(List(Some(1), None)) == None)
    assert(optionMonad.sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
  }

  test("EXERCISE 11.3 traverse") {
    import Monad._
    assert(optionMonad.traverse(List(None))(a => a) == None)
    assert(optionMonad.traverse(List(Some(1), None))(a => a) == None)
    assert(optionMonad.traverse(List(Some(1), Some(2)))(a => a) == Some(List(1, 2)))
    assert(optionMonad.traverse(List(1, 2))(a => Some(a * 2)) == Some(List(2, 4)))
  }
}
