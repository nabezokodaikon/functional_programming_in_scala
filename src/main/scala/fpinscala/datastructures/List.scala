package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /*
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  */

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](h: A, l: List[A]) = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /*
   * リスト全体を最期の要素までコピーしているため、
   * tailのように一定時間で実装できない。
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t)) // リストを生成し直している。
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  /*
   * EXERCISE 3.7
   * リスト内に0.0を検出しても、
   * foldRightはリストの最期までたどるため、
   * 処理を中止することはできない。
   */
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumByFoldLeft(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productByFoldLeft(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthByFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRightViaFoldLeft(l, r)(Cons(_, _))

  /*
   * `append`は最初の引数に比例する時間を要する。
   * `foldRight`はリストの最期まで走査してから、畳込みを開始する。
   * よって、全てのリストの全長に線形になる。
   */
  def concat[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft_1(l, List[A]())(appendViaFoldRight)

  def concatWithPrint[A](l: List[List[A]]): List[A] =
    foldRightViaFoldLeft_1(l, List[A]())((a, b) => {
      println(s"append a: ${a}, b: ${b}")
      appendViaFoldRight(a, b)
    })

  def increment(l: List[Int]): List[Int] =
    foldRightViaFoldLeft_1(l, List[Int]())((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft_1(l, List[String]())((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft_1(as, List[B]())((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft_1(as, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

  /*
   * mapと同じような働きをする。
   * 単一の結果ではなくリストを返し、
   * そのリストは最終的な結果のリストに挿入される。
   * <例>
   * flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3)
   *
   * 二重の配列を一重にする。
   * <例>
   * flatMap(List(List[1, 2], List[3, 4]))(i => List(i)) == List(1, 2, 3, 4)
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
}
