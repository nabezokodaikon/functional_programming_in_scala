package fpinscala
package applicative

import monads.Functor
import state._
import State._
import monoids._
import Monoid.Foldable
import language.higherKinds
import language.implicitConversions

// List 12-1
trait Applicative[F[_]] extends Functor[F] {
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

  // EXERCISE 12.2 sequenceMap
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) {
      case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v))
    }

  // EXERCISE 12.1 replicateM
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // EXERCISE 12.1 product
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // EXERCISE 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this

    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {

      def unit[A](a: => A) =
        (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this

    new Applicative[({ type f[x] = F[G[x]] })#f] {

      def unit[A](a: => A) =
        self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }

  }

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match { case (a, (b, c)) => ((a, b), c) }

  // EXERCISE 12.2 map2ViaApply
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // EXERCISE 12.2 mapViaApply
  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // EXERCISE 12.2 applyViaMap2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))
  // map2(fab, fa)((a, b) => a(b))

  // EXERCISE 12.3 map3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  // EXERCISE 12.3 map4
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
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

object Monad {

  // EXERCISE 12.5
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]) =
        eea match {
          case Right(a) => f(a)
          case Left(e) => Left(e)
        }
    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}

object Applicative {

  // List 12-5
  val streamApplicative = new Applicative[Stream] {

    // 無限の定数ストリーム。
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    // 要素を各箇所で結合。
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  val optionApplicative = new Applicative[Option] {

    def unit[A](a: => A): Option[A] = Some(a)

    override def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        aa <- a
        bb <- b
      } yield f(aa, bb)
  }

  val listApplicative = new Applicative[List] {

    def unit[A](a: => A): List[A] = List(a)

    override def map2[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
      a zip b map f.tupled
  }

  // EXERCISE 12.6
  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {

      def unit[A](a: => A) = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e @ Failure(_, _), _) => e
          // case (Failure(a, b), _) => Failure(a, b)
          case (_, e @ Failure(_, _)) => e
          // case (_, Failure(a, b)) => Failure(a, b)
        }
    }

  // List 12-10
  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }

  // List 12-7
  case class WebForm(name: String, birthdate: java.util.Date, phoneNumber: String)

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone)
    )(
        WebForm(_, _, _)
      )

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, java.util.Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone bumber must be 10 digits")
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  // List 12-11
  import Applicative._
  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))

  // List 12-12
  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  // List 12-15
  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)

  def toListViaMapAccum[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndexViaMapAccum[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  // List 12-13
  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => (for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i))).run(0)._1

  // List 12-14
  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]] // 現在の要素(蓄積リスト)を取得。
      _ <- set(a :: as) // 現在の要素を追加し、新しいリストを新しい状態として設定。
    } yield ())).run(Nil)._2.reverse
}

object Traverse {

  // EXERCISE 12.13 listTraverse
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  // EXERCISE 12.13 optionTraverse
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  // EXERCISE 12.13 treeTraverse
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}

// EXERCISE 12.13 treeTraverse
case class Tree[+A](head: A, tail: List[Tree[A]])

// List 12-6
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]
