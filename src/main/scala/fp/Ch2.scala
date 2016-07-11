package fp

import scala.annotation.tailrec

// This class represents Chapter 2 exercises of Functional Programming in Scala
object Ch2 {
  val FIBS_INITIAL = Set(0, 1)
  // Exercise p.22 --> Provide the nth Fibonacci number with a tail recursive function
  def fib(n: Int): Int = {
    @tailrec
    def fibRecursive(first: Int, second: Int, count: Int): Int = {
      if (count == 2) first + second
      else fibRecursive(second, first + second, count - 1)
    }
    if(FIBS_INITIAL contains n) n else fibRecursive(0, 1, n)
  }

  // Exercise p.29 --> verify if an Array is sorted according to an ordering function
  def isSorted[@specialized(Int,Double,Long) A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    as.forall { i =>
      val iPos = as.indexOf(i)
      iPos == as.length - 1 ||
        gt(i, as(iPos + 1))
    }

  // NTH composeAll, apply a List of functions in a composite way
  def composeAll[A](fs: List[A => A]): A => A =
    fs.foldLeft((a: A) => identity(a))(_ andThen _)

  // Exercise p.29 --> implement partial1
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)
  
  // Exercise p.30 --> curry. Return a function that takes its arguments one by one
  def curry[A,B,C](f: (A,B) => C): A =>(B => C) =
    a => b => f(a,b)
  
  // Exercise p.30 --> uncurry. Return a function that takes all of its arguments together.
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a,b) => f(a)(b)
  
  // Exercise p.30 --> compose. Compose two functions to get only one by applying f after applying g
  def compose[A,B,C](f: B => C, g:A => B): A => C =
    a => f(g(a))
}
