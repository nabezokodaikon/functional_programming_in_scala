package fpinscala.parallelism

import java.util.concurrent._

/*
 * java.util.concurrent.ExecutorserviceをScalaで表現
 *
class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRUnning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}
*/

/*
case class Par[A](a: A) {

  def flatMap[B](f: A => Par[B]): Par[B] =
    f(a)

  def map[B](f: A => B): Par[B] =
    flatMap(a => Par.unit(f(a)))
}
*/

object Par {

  type Par[A] = ExecutorService => Future[A]

  // 直ちにa値が得られる計算を作成。
  // 定数値を並列計算に昇格させる。
  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // 2つの並列計算の結果を2項関数で結合。
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // runによる並列評価の対象としてマーク。
  // この評価はrunによって強制されるまで実際には発生しない。
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // 式aをrunによる並列評価のためにラッピング。
  // 並列評価の対象としてマークする。
  // def lazyUnit[A](a: => A): Par[A] =
  // fork(unit(a))

  // 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得。
  // 実際に計算を行うことで、Parから値を取得する。
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] =
    a(s)

  /*
  def get[A](a: Par[A]): A =
    a.a

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

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
    */
}

object Examples {
}
