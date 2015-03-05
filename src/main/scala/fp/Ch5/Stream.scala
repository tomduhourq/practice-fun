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

  val ones: Stream[Int] = cons(1, ones)

  // Exercise 7 p.73 --> constant: generalisation of ones
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 8 p.73 --> from: create a Stream adding 1 for each element generated
  def from(n: Int): Stream[Int] = cons(n,from(n+1))

  // Exercise 9 p.73 --> fibs: Generate an infinite Stream of Fibonacci numbers
  lazy val fibs: Stream[Int] = {
    def fibRecursive(prev: Int, fib1: Int): Stream[Int] =
      cons(prev, fibRecursive(fib1, prev + fib1))
    fibRecursive(0, 1)
  }

  // Exercise 10 p.73 --> unfold: create an infinite Stream from applying the function to the initial state
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }
}
