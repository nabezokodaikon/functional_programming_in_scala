package fpinscala.parallelism

case class Par[A](a: A) {
  import Par._

  def flatMap[B](f: A => Par[B]): Par[B] =
    f(a)

  def map[B](f: A => B): Par[B] =
    flatMap(a => unit(f(a)))
}

object Par {

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  def unit[A](a: A): Par[A] =
    Par[A](a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def get[A](a: Par[A]): A =
    a.a

  def run[A](a: Par[A]): A =
    a.a

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def fork[A](a: => Par[A]): Par[A] =
    a

  def sum_2(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum_2(l))
      val sumR: Par[Int] = Par.unit(sum_2(r))
      Par.get(sumL) + Par.get(sumR)
    }

  def sum_3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum_3(l), sum_3(r))(_ + _)
    }

  def sum_4(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum_4(l)), Par.fork(sum_4(r)))(_ + _)
    }
}

object Examples {
}
