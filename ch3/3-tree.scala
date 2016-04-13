sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1 // left + right + parent
    }
  }
}

val testTree = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

Tree.size(testTree)
