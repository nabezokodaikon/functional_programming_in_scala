package fpinscala.parallelism

import java.util.concurrent._

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
  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  // 与えられたParを完全に評価し、forkによって要求される並列計算を生成し、結果の値を取得。
  // 実際に計算を行うことで、Parから値を取得する。
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] =
    a(s)

  // EXERCISE 7.4
  def asyncF[A, B](f: A => B): A => Par[B]
    a => lazyUnit(f(a))
}

object Examples {
}
