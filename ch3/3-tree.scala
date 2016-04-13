sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /* Exercise 3.25 */
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1 // left + right + parent
    }
  }

  /* Exercise 3.26 */
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(i) => i
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  /* Exercise 3.27 */
  def depth[A](tree: Tree[A]): Int = {
    def go(tree: Tree[A], d: Int): Int = {
      tree match {
        case Leaf(_) => d
        case Branch(l, r) => go(l, d + 1) max go(r, d + 1)
      }
    }
    go(tree, 0)
  }
}

val testTree = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

Tree.size(testTree)
Tree.maximum(testTree)
Tree.depth(testTree)
