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

  /*
   * 2つの並列計算の結果を2項関数で結合。
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

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
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val i = run(es)(n).get
      run(es)(choices(i))
    }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  // flatMap
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(pa).get
      run(es)(choices(k))
    }

  def choiceViaChooser[A](b: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(b)(b => if (b) t else f)

  def choiceNViaChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(p)(i => choices(i))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    chooser(a)(pa => pa)

}

object Examples {
}
