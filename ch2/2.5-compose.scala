/** Exercise 2.5: Function composition **/
def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

// def add_1(n: Int): Int =
//   n + 1

// def multiply_by_2(n: Int): Int =
//   n * 2

// compose(multiply_by_2, add_1)(2)  // Evaluates right to left, I guess
