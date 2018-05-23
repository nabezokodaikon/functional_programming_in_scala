package fpinscala.laziness

trait Stream[+A] {

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
}
