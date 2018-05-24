package fpinscala.laziness

trait Stream[+A] {
  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List[A]()
    }

  def toList_2: List[A] = {

    def loop(s: Stream[A], l: List[A]): List[A] =
      s match {
        case Cons(h, t) => loop(t(), h() :: l)
        case _ => l
      }

    loop(this, List()).reverse
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((h, t) =>
      if (f(h)) cons(h, t)
      else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append (t))

  def find(f: A => Boolean): Option[A] =
    filter(f).headOptionViaFoldRight
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant_2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {

    def loop(p1: Int, p2: Int): Stream[Int] = {
      cons(p1, loop(p2, p1 + p2))
    }

    loop(0, 1)
  }
}
