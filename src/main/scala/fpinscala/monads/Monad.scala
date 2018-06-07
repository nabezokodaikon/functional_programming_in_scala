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

  // List 11-4
  def condistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {

  // List 11-2 FunctorをListに適用。
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      as map f
  }

}

trait Mon[F[_]] {

  // List 11-7 トレイトへのmapとflatMapの追加。
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // List 11-6 map2のためのMonトレイト。
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

}

object Mon {

  val optionMon = new Mon[Option] {
    def map[A, B](o: Option[A])(f: A => B): Option[B] =
      o map f

    def flatMap[A, B](o: Option[A])(f: A => Option[B]): Option[B] =
      o flatMap f
  }
}
