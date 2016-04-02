case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) {

  def prefix: String = {
    def prefixRec[A](left: Tree[A]): String = left match {
      case Tree(n, None, None) => n toString
      case Tree(n, Some(l), None) => n.toString + prefixRec(l)
      case Tree(n, None, Some(r)) => n.toString + prefixRec(r)
      case Tree(n, Some(l), Some(r)) => n.toString + prefixRec(l) + prefixRec(r)
    }
    prefixRec(this)
  }

  def infix: String = {
    def infixRec[A](left: Tree[A]): String = left match {
      case Tree(n, None, None) => n toString
      case Tree(n, Some(l), None) => infixRec(l) + n.toString
      case Tree(n, None, Some(r)) => n.toString + infixRec(r)
      case Tree(n, Some(l), Some(r)) => infixRec(l) + n.toString + infixRec(r)
    }
    infixRec(this)
  }

  def postfix: String = {
    def postfixRec[A](left: Tree[A]): String = left match {
      case Tree(n, None, None) => n toString
      case Tree(n, Some(l), None) => postfixRec(l) + n.toString
      case Tree(n, None, Some(r)) => postfixRec(r) + n.toString
      case Tree(n, Some(l), Some(r)) => postfixRec(l) + postfixRec(r) + n.toString
    }
    postfixRec(this)
  }
}

/**
  *           2
  *         /   \
  *       3     7
  *     /  \
  *   6     9
  */
val six = Tree(6, None, None)
val nine = Tree(9, None, None)
val three = Tree(3, Some(six), Some(nine))
val seven = Tree(7, None, None)
val root = Tree(2, Some(three), Some(seven))

root.prefix == "23697"
root.infix == "63927"
root.postfix == "69372"