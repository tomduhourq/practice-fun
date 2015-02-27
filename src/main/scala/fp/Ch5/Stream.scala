package fp.Ch5

import Stream._
trait Stream[+A] {

  // Exercise 1 p.69 --> toList: force evaluation on each element of the Stream, yielding a List
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => Nil
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  // Create a stream from a head and another Stream.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
  
  // Create a stream given a vararg of type A
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
