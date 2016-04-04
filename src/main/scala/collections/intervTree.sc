case class Tree[+A](value: A, left: Option[Tree[A]], right: Option[Tree[A]]) {

  def map[V](f: A => V): Tree[V] =
  //  map option and then the Tree inside
   Tree(f(value), left.map(l => l.map(f)), right.map(r => r.map(f)))

  def childrenLeftRight: List[Tree[A]] = List(left, right).flatten

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

def dfs[A](tree: Tree[A]): List[A] = {
  var output = List[A]()
  tree.map(t => (output = t :: output))
  output reverse
}

def bfs[A](tree: Tree[A]): List[A] = {
  @annotation.tailrec
  def bfsLoop(accum: List[List[A]], nextLayer: List[Tree[A]]): List[A] =
    nextLayer match {
      // reached lower level => "dequeue" all
      case Nil => accum.reverse.flatten
      // "enqueue" the current level and get next layer
      case _ => bfsLoop(
        nextLayer.map(_.value) :: accum,
        nextLayer.flatMap(_.childrenLeftRight)
      )
    }
  bfsLoop(List[List[A]](), List(tree))
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

dfs(root).mkString("") == "23697"
bfs(root).mkString("") == "23769"