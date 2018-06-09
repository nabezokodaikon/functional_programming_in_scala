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

// List 11-8 Monad
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  // EXERCISE 11.8 composeベースのflatMap。
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  // 末尾再帰でないため駄目。
  def sequence_2[A](lma: List[F[A]]): F[List[A]] =
    lma match {
      case Nil => unit(Nil)
      case h :: t => map2(h, sequence_2(t))(_ :: _)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((h, t) => map2(f(h), t)(_ :: _))

  // 末尾再帰でないため駄目。
  def traverse_2[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la match {
      case Nil => unit(Nil)
      case h :: t => map2(f(h), traverse_2(t)(f))(_ :: _)
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(List[A]())
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }

  // EXERCISE 11.7 クライスリ合成関数。
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
  // a => flatMap(f(a))(b => g(b))
}

object Monad {

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
  }
}
