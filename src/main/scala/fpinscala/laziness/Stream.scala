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

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), c) if c > 1 => Some((h(), (t(), c - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A], t()))
      case (Cons(h, t), Cons(h2, t2)) => Some(f(Some(h()), Some(h2())) -> (t(), t2()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)((_, _)).takeWhileViaUnfold(!_._2.isEmpty).forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](sub: Stream[A]): Boolean =
    tails exists (_ startsWith sub)

  /*
   * unfoldは左から右へStreamの要素を生成するため、unfoldでの実装は不可。
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case _ => empty
    }

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  val fibsViaUnfold_2: Stream[Int] =
    unfold((0, 1)) { case (s1, s2) => Some((s1, (s2, s1 + s2))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))
}
