package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds

trait Functor[F[_]] {

  // List 11-1 mapを実装するデータ型。
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // List 11-3 F[(A, B)]が定義されていて、Fをペアに分配して(F[A], F[B])を得る。(unzip)
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
}

object Functor {

  // List 11-2 FunctorをListに適用。
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      as map f
  }

}
