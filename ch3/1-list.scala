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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

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

  /* Exercise 3.5 */
  def head[A](as: List[A]): A = as match {
    // case Nil => ???   // Is there a null type I should use here?
    case Cons(h, t) => h
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def go(l: List[A], f: A => Boolean): List[A] = {
      if (l == Nil) Nil
      else if (f(head(l))) go(tail(l), f)
      else Cons(head(l), go(tail(l), f))
    }
    go(l, f)
  }

  /* Exercise 3.6 */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /*Exercise 3.9 */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + y)
  }

  /* Exercise 3.10 */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  /* Exercise 3.11 */
  /* I can't get these to work for undefined types. Will come back later */
  // def sumLeft[A](as: List[A]) = {
  //   foldLeft(as, 0)((a1: A, a2: A) => a1.+(a2))
  // }

  // def productLeft[A](as: List[A]) = {
  //   foldLeft(as, 1)(_ * _)
  // }

  def lengthLeft[A, B](as: List[A]): Int =
    foldLeft(as, 0)((n: Int, a: A) => n + 1)

  /* Exercise 3.12 */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((xs, x) => Cons(x, xs))
  }

  /* Exercise 3.13 */
  def foldLeftByRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldRight(reverse(as), z)(f)
  }

  def foldRightByLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldLeft(reverse(as), z)(f)
  }
}

val seq5i = List(1,2,3,4,5)

// List.tail(seq5i)
// List.setHead(seq5i, 100)
// List.drop(seq5i, 3)
// List.dropWhile(seq5i, (x: Int) => x % 2 == 0)
// List.init(seq5i)
// List.length(seq5i)
// List.foldLeft(seq5i, 1)(_ * _)
// List.lengthLeft(seq5i)
// List.reverse(seq5i)
List.foldLeftByRight(seq5i, 1)(_ * _)
List.foldRightByLeft(seq5i, 1)(_ * _)
