/** Exercise 2.3 **/
def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => ((b: B) => f(a, b))

/** Exercise 2.4 **/
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

// def add(n1: Int, n2: Int): Int =
//   n1 + n2

// curry(add)(5)(4)

// uncurry(curry(add))(5, 4)
