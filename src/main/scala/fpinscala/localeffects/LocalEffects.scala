package fpinscala.localeffects

import fpinscala.monads._

object Mutable {

  // List 14-1
  def quicksort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) xs
    else {
      val arr = xs.toArray

      // 配列内の2つの要素を交換。
      def swap(x: Int, y: Int) = {
        val tmp = arr(x)
        arr(x) = arr(y)
        arr(y) = tmp
      }

      // 配列内の一部をpivotよりも小さい要素と大きい要素にそれぞれ分割。
      def partition(n: Int, r: Int, pivot: Int) = {
        val pivotVal = arr(pivot)
        swap(pivot, r)
        var j = n
        for (i <- n until r) {
          if (arr(i) < pivotVal) {
            swap(i, j)
            j += 1
          }
        }
        swap(j, r)
        j
      }

      // 配列の一部を直接ソート。
      def qs(n: Int, r: Int): Unit = {
        if (n < r) {
          val pi = partition(n, r, n + (r - n) / 2)
          qs(n, pi - 1)
          qs(pi + 1, r)
        }
      }

      qs(0, arr.length - 1)
      arr.toList
    }
  }

  // List 14-2
  sealed trait ST[S, A] { self =>

    protected def run(s: S): (A, S)

    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  // List 14-2
  object ST {

    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S) = (memo, s)
      }
    }

    // List 14-5
    def runST[A](st: RunnableST[A]): A =
      st.apply[Unit].run(())._1
  }

  // List 14-3
  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
      }
    }
  }

  // List 14-3
  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      var cell = a
    })
  }

  // List 14-4
  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  // List 14-6
  // Scalaで，配列の構築に`implicit manifest`が必要。
  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {

    protected def value: Array[A]

    def size: ST[S, Int] = ST(value.size)

    // 配列の指定されたインデックス一に値を書き込む。
    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }

    // 配列の指定されたインデックス一の値を読み取る。
    def read(i: Int): ST[S, A] = ST(value(i))

    // 配列をミュータブルなリストに変換。
    def freeze: ST[S, List[A]] = ST(value.toList)

    // EXERCISE 14.1
    def fill(xs: Map[Int, A]): ST[S, Unit] =
      xs.foldRight(ST[S, Unit](Unit)) {
        case ((k, v), st) => st flatMap (_ => write(k, v))
      }

    // List 14-8
    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
  }

  object STArray {
    // vの値が含まれた、指定されたサイズの配列を作成。
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = Array.fill(sz)(v)
      })

    // List 14-7
    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
      ST(new STArray[S, A] {
        lazy val value = xs.toArray
      })
  }
}

object Immutable {
  import Mutable._

  def noop[S] = ST[S, Unit](())

  // EXERCISE 14.2
  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    vp <- a.read(pivot)
    _ <- a.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      _ <- s
      vi <- a.read(i)
      _ <- if (vi < vp) (for {
        vj <- j.read
        _ <- a.swap(i, vj)
        _ <- j.write(vj + 1)
      } yield ())
      else noop[S]
    } yield ())
    x <- j.read
    _ <- a.swap(x, r)
  } yield x

  // EXERCISE 14.2
  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] =
    if (l < r)
      for {
        pi <- partition(a, l, r, l + (r - l) / 2)
        _ <- qs(a, l, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else
      noop[S]

  // List 14-9
  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty)
      xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })
}

// EXERCISE 14.3
import scala.collection.mutable.HashMap

sealed trait STMap[S, K, V] {
  import Mutable.ST

  protected def table: HashMap[K, V]

  def size: ST[S, Int] = ST(table.size)

  def apply(k: K): ST[S, V] = ST(table(k))

  def get(k: K): ST[S, Option[V]] = ST(table.get(k))

  def +=(kv: (K, V)): ST[S, Unit] = ST(table += kv)

  def -=(k: K): ST[S, Unit] = ST(table -= k)
}

object STMap {
  import Mutable.ST

  def emtpy[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val table = HashMap.empty[K, V]
  })

  def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
    val table = (HashMap.newBuilder[K, V] ++= m).result
  })
}
