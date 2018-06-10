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

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  // EXERCISE 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(a => f(a)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

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
}

object Monad {

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    override def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }
}

object StateMonads {
  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    override def flatMap[A, B](s: IntState[A])(f: A => IntState[B]): IntState[B] = s flatMap f
  }

  def stateMonad[S] = new Monad[({ type lambda[x] = State[S, x] })#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = s flatMap f
    def getState[S]: State[S, S] = State(s => (s, s))
    def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))
  }

  def getState[S]: State[S, S] = State(s => (s, s))
  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))

  val F = stateMonad[Int]

  def zipWidthIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }

  def ask[R]: Reader[R, R] = Reader(r => r)
}
