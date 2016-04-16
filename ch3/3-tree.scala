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
    tree match {
      case Leaf(x) => 0
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }
  }

  /* Exercise 3.28 */
  def map[A](tree: Tree[A])(f: A => A): Tree[A] = {
    tree match {
      case Leaf(x) =>  Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  /* Exercise 3.29 */
  def fold[A,B,C](tree: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B = {
    tree match {
      case Leaf(x) => fLeaf(x)
      case Branch(l, r) => fBranch(fold(l)(fLeaf)(fBranch),
                                   fold(r)(fLeaf)(fBranch))
    }
  }

  def sizeViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(x => 1)((l, r) => l + r + 1)
  }

  def maximumViaFold(tree: Tree[Int]): Int = {
    fold(tree)(x => x)((l, r) => l max r)
  }

  def depthViaFold[A](tree: Tree[A]): Int = {
    fold(tree)(x => 0)((l, r) => (l max r) + 1)
  }

  def mapViaFold[A](tree: Tree[A])(f: A => A): Tree[A] = {
    fold(tree)(x => Leaf(f(x)): Tree[A])((l, r) => Branch(l, r))
  }
}

val testTree = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))

Tree.size(testTree)
Tree.maximum(testTree)
Tree.depth(testTree)
Tree.map(testTree)(_ * 2)
Tree.sizeViaFold(testTree)
Tree.maximumViaFold(testTree)
Tree.depthViaFold(testTree)
Tree.mapViaFold(testTree)(_ * 2)
