// Write a function to reverse a single linked list
def reverse[T](l: List[T]) = l reverse
def reverse2[T](l: List[T]): List[T] = {
  def reverseRec[T](left: List[T]): List[T] =
    left match {
      case Nil => Nil
      case head :: tail => reverseRec(tail) ::: List(head)
    }
  reverseRec(l)
}
reverse(List(1,2,3))
reverse2(List(1,2,3))
reverse(Nil)
reverse2(Nil)

// Singly linked list

trait MyList[+A]
case class Node[A](v: A, next: MyList[A]) extends MyList[A] {
  override def toString = v.toString + " -> " + next.toString
}
case object Empty extends MyList[Nothing] {
  override def toString = "Null"
}
object MyList {
  def reverse[A](l: MyList[A]): MyList[A] = {
    @annotation.tailrec
    def reverseRec(left: MyList[A], acum: MyList[A]): MyList[A] =
      left match {
        case Empty => acum
        case Node(v, next) => reverseRec(next, Node(v, acum))
      }
    reverseRec(l, Empty)
  }
}
val l = Node(2, Node(3, Node(5, Empty)))
MyList.reverse(l)



