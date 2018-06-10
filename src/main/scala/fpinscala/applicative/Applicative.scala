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

  // EXERCISE 12.3 map3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  // EXERCISE 12.3 map4
  def map3[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}

// List 12-2
trait Monad[F[_]] extends Applicative[F] {
  // Monadの実装では、少なくとも、unitを実装し、
  // flatMapかjoinのいずれかとmapを上書きしなければならない。

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}
