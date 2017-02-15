package gettingstarted02

/**
  * fibonacci_2_1
  */
object fibonacci_2_1 {

  def fib(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  def fibTailRec(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, n: Int, p2: Int, p1: Int): Int = {
      if (i == n) p2 + p1
      else go(i + 1, n, p1, p1 + p2)
    }

    if (n == 0) 0
    else if (n == 1) 1
    else go(2, n, 0, 1)
  }
}
