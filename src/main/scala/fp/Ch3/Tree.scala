package fp.Ch3

/**
 * Created by tomas on 22/02/15.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Exercise 25 p.48 --> size: Give the number of nodes which a tree has
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  // Exercise 26 p.48 --> maximum
  def maximum(t: Tree[Int]): Int = {
    def realMax(remTree: Tree[Int], actualMax: Int): Int = remTree match {
      case Leaf(x) => x max actualMax
      case Branch(l,r) => realMax(l,actualMax) max realMax(r,actualMax)
    }
    realMax(t,-99999)
  }

  // Exercise 27 p.48 --> depth: Give the longest path from the root of a tree to any leaf.
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + depth(l) max 1 + depth(r)
  }

  // Exercise 28 p.48 -->  mapT: apply a function to all elements of a Tree
  def mapT[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(mapT(l)(f),mapT(r)(f))
  }

  // Exercise 29 p.49 --> fold: Generalisation of size,max and depth logic in one function.
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeByFold[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)
  def maximumByFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depthByFold[A](t: Tree[A]): Int = fold(t)(a => 0)((branch1,branch2) => 1 + (branch1 max branch2))
  def mapByFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
