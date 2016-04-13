/* Exercise 3.24 */
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  def go(n: Int): Boolean = {
    if (n > sup.length) false
    else if (sup.take(n).takeRight(sub.length) == sub) true
    else go(n+1)
  }
  go(0)
}

hasSubsequence(List(1,2,3,4,5), List(2,3,4))
hasSubsequence(List(1,2,3,4,5), List(2,3,5))
