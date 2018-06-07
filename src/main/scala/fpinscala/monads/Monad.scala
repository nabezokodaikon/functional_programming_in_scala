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

}
