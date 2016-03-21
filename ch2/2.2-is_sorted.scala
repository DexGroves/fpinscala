/** Exercise 2.2: Check if an array is sorted according to some comparator **/
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean =
    if (n >= as.length) true
    else if (!ordered(as(n), as(n - 1))) false
    else loop(n + 1)
  loop(1)
}

// def isOrderedInteger(n1: Int, n2: Int): Boolean = {
//   n2 <= n1
// }

// isSorted(Array(1,2,3,4,5), isOrderedInteger)
// isSorted(Array(6,5,4,3,2), isOrderedInteger)
