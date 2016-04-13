/* Exercise 3.24 */
def hasSubsequence[A](search: List[A], term: List[A]): Boolean = {
  def go(n: Int): Boolean = {
    if (n > search.length) false
    else if (search.take(n).takeRight(term.length) == term) true
    else go(n+1)
  }
  go(0)
}

hasSubsequence(List(1,2,3,4,5), List(2,3,4))
hasSubsequence(List(1,2,3,4,5), List(2,3,5))
