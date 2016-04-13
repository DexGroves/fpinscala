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
    case Nil => head(as)
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

  /* Exercise 3.14 */
  def append[A](as: List[A], z: A): List[A] = {
    as match {
      case Nil => as
      case Cons(x, Nil) => Cons(x, Cons(z, Nil))
      case Cons(x, xs) => Cons(x, append(xs, z))
    }
  }

  def appendList[A, B](as: List[A], z: List[A]): List[A] = {
    as match {
      case Nil => z
      case Cons(x, Nil) => Cons(x, z)
      case Cons(x, xs) => Cons(x, appendList(xs, z))
    }
  }

  /* Exercise 3.15 */
  /* I don't know if this satisfies the linear time requirement */
  def listConcat[A](as: List[List[A]]): List[A] = {
    foldLeft(as, List[A]())(appendList)
  }

  /* Neverending pages of exercises */
  /* Exercise 3.16 */
  def addOneToListOfIntegers(ints: List[Int]): List[Int] = {
    ints match {
      case Nil => ints
      case Cons(i, Nil) => Cons(i+1, Nil)
      case Cons(i, is) => Cons(i+1, addOneToListOfIntegers(is))
    }
  }

  /* Exercise 3.17 */
  def doubleToString(ds: List[Double]): List[String] = {
    foldRight(ds, Nil:List[String])((d, ds) => Cons(d.toString, ds))
  }

  /* Exercise 3.18 */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((x, xs) => Cons(f(x), xs))
  }

  /* Exercise 3.19 */
  /* already did this as dropWhile */

  /* Exercise 3.20 */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    // foldRight(as, Nil:List[B])(xs => appendList(f(x), flatMap(xs)(f)))
    listConcat(map(as)(f))
  }

  /* Exercise 3.21 */
  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x: A) => if (f(x)) Cons(x, Nil) else Nil:List[A])
  }

  /* Exercise 3.22 */
  def listAdd(lhs: List[Int], rhs: List[Int]): List[Int] = {
    (lhs, rhs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh + rh, listAdd(lt, rt))
    }
  }

  /* Exercise 3.33 */
  def zipWith[A](lhs: List[A], rhs: List[A])(f: (A, A) => A): List[A] = {
    (lhs, rhs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt)(f))
    }
  }
}

val seq5i = List(1, 2, 3, 4, 5)
val seq5d = List(1.1, 2.2, 3.3, 4.4, 5.5)

// List.tail(seq5i)
// List.setHead(seq5i, 100)
// List.drop(seq5i, 3)
// List.dropWhile(seq5i, (x: Int) => x % 2 == 0)
// List.init(seq5i)
// List.length(seq5i)
// List.foldLeft(seq5i, 1)(_ * _)
// List.lengthLeft(seq5i)
// List.reverse(seq5i)
// List.foldLeftByRight(seq5i, 1)(_ * _)
// List.foldRightByLeft(seq5i, 1)(_ * _)
// List.append(seq5i, 6)
// List.appendList(seq5i, seq5i)
// List.listConcat(List(seq5i, seq5i, seq5i))
// List.addOneToListOfIntegers(seq5i)
// List.doubleToString(seq5d)
// List.map(seq5d)(((d: Double) => d.toString))
// List.dropWhile(seq5i, (x: Int) => x % 2 == 1)
// List.flatMap(List(1, 2, 3))(i => List(i, i))
// List.flatMapFilter(seq5i)((x: Int) => x % 2 == 1)
// List.listAdd(List(1,2,3), List(4,5,6))
// List.zipWith(List(1,2,3), List(4,5,6))(_ + _)
