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
}
