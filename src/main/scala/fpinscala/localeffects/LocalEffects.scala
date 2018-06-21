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
}
