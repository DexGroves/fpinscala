sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /* Exercise 3.2 */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  /* Exercise 3.3 */
  def setHead[A](as: List[A], r: A): List[A] = as match {
    case Nil => Cons(r, Nil)
    case Cons(h, t) => Cons(r, t)
  }

  /* Exercise 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = {
    def go(l: List[A], n: Int): List[A] = {
      if (n <= 1) tail(l)
      else go(tail(l), n - 1)
    }
    go(l, n)
  }
}

val seq5i = List(1,2,3,4,5)

// List.sum(seq5i)

// List.tail(seq5i)
// List.setHead(seq5i, 100)
// List.drop(seq5i, 3)
