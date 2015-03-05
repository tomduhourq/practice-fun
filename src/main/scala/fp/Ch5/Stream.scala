package fp.Ch5

import Stream._
trait Stream[+A] {

  // Exercise 1 p.69 --> toList: force evaluation on each element of the Stream, yielding a List
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => Nil
  }

  // Exercise 2 p.69 --> take: Return a List of the first n elements of a Stream
  def take(n: Int): Stream[A] = this match {
    case Cons(h,_) if n == 1 => Stream.cons(h(),empty)
    case Cons(h,t) if n > 1 => Stream.cons(h(),t().take(n-1))
    case _ => empty
  }

  // Exercise 3 p.70 --> takeWhile: return a Stream until the condition is not given
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(),t().takeWhile(f))
    case _ => empty
  }

  // NTH --> foldRight
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // NTH --> exists
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  // Exercise 4 p.71 --> forAll
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  // Exercise 5 p.71 --> takeWhileByFoldRight
  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a,b) => if(p(a)) cons(a,b) else Empty)

  // Exercise 6 p.71 --> map, filter, append and flatMap
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a,b) => cons(f(a),b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a,b) => if(f(a)) cons(a,b) else b)

  def append[B >: A](stream: => Stream[B]): Stream[B] =
    foldRight(stream)((a,b) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)


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
