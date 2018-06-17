package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds

trait Functor[F[_]] {

  // List 11-1
  def map[A, B](fa: F[A])(f: A => B): F[B]

  // List 11-3
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  // List 11-4
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {

  // List 11-2
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

}

// List 11-8
trait Monad[F[_]] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(a => a)
  // lma.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // EXERCISE 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }

  // EXERCISE 11.7
  def composeViaFlatMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
  // a => flatMap(f(a))(b => g(b))

  // EXERCISE 11.8
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    composeViaFlatMap((_: Unit) => ma, f)(())

  // EXERCISE 11.12
  def joinViaFlatMap[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  // EXERCISE 11.13
  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
    joinViaFlatMap(map(ma)(f))

  // EXERCISE 11.13
  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => joinViaFlatMap(map(f(a))(g))
}

object Monad {

  // EXERCISE 11.1
  def listMonad = new Monad[List] {

    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  // EXERCISE 11.1
  def optionMonad = new Monad[Option] {

    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  // EXERCISE 11.1
  def streamMonad = new Monad[Stream] {

    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }
}

// EXERCISE 11.7
case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {

  val idMonad = new Monad[Id] {

    def unit[A](a: => A) = Id(a)

    def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida.flatMap(f)
  }
}

object IntStateMonad extends Monad[({ type f[A] = State[Int, A] })#f] {
  type IntState[A] = State[Int, A]

  def unit[A](a: => A): IntState[A] = State(s => (a, s))

  def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
    st flatMap f
}

object StateMonad {

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {

    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def getState[S]: State[S, S] = State(s => (s, s))
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))

  // List 11-12
  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse
}

// EXERCISE 11.20
case class Reader[R, A](run: R => A)

object Reader {

  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {

    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }

  def ask[R]: Reader[R, R] = Reader(r => r)
}
