package fpinscala
package applicative

import monads.Functor
import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions

// List 12-1
trait Applicative[F[_]] extends Functor[F] {
  // プリミティブコンビネータ
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // 派生コンビネータ
  // mapはunitとmap2を使って実装できる。
  // unit(())がダミー値()でunitを呼び出している。
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // EXERCISE 12.1 sequence
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  // EXERCISE 12.1 replicateM
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // EXERCISE 12.1 product
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // EXERCISE 12.2 map2ViaApply
  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // EXERCISE 12.2 mapViaApply
  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // EXERCISE 12.2 applyViaMap2
  def applyViaMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))
  // map2(fab, fa)((a, b) => a(b))
}
