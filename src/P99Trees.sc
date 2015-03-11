
sealed abstract class Tree[+T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T](x: U)(implicit ev1: U => Ordered[U]): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
//  def this(value: T) = this(value, End, End)

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  def isMirrorOf[V](n: Tree[V]) = n match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case _ => false
  }

  def isSymmetric: Boolean = left.isMirrorOf(right)

  def addValue[U >: T](x: U)(implicit ev1: U => Ordered[U]): Tree[U] =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))
}

case object End extends Tree[Nothing] {
  override def toString = "."

  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true
  def addValue[U](x: U)(implicit ev1: U => Ordered[U]): Tree[U] = Node(x, End, End)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 =>
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    case n if n % 2 == 0 =>
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
  }

  def fromList[T](elements: List[T])(implicit ev1: T => Ordered[T]): Tree[T] =
    elements.foldLeft(End: Tree[T])((tree, value) => tree.addValue(value))
}

Tree.cBalanced(2, "A")
Node(2, Node(1), Node(3)).addValue(6)
Tree.fromList(List(5, 3, 18, 1, 4, 12, 21))
Tree.fromList(List(3, 2, 5, 7, 4))
