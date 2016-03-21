/** Exercise 2.1: Calculate the nth Fibonacci number **/
def fib(n: Int): Int = {
  @annotation.tailrec
  def go (remaining: Int, current: Int, last: Int): Int = {
    if (remaining <= 1) last
    else go(remaining - 1, last, last + current)
  }

  if (n < 1) return 0
  go(n, 1, 0)
}

// fib(5)
