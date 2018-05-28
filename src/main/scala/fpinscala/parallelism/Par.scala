package fpinscala.parallelism

case class Par[A](a: A) {
}

object Par {

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  def unit[A](a: => A): Par[A] =
    Par[A](a)

  def get[A](a: Par[A]): A =
    a.a

  def sum_2(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum_2(l))
      val sumR: Par[Int] = Par.unit(sum_2(r))
      Par.get(sumL) + Par.get(sumR)
    }
}

object Examples {
}
