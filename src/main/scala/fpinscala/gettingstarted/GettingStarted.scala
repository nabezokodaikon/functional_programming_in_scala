package fpinscala.gettingstarted

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) =
    s"The absolute value of ${x} is ${abs(x)}"

  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {

    @annotation.tailrec
    def loop(n: Int, prev: Int, current: Int): Int = {
      if (n == 0) prev
      else loop(n - 1, current, prev + current)
    }

    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
