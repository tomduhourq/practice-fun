package fp.Ch3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/** Chapter 3 */
object List {

  def apply[A](elems: A*): List[A] = if (elems.isEmpty) Nil else Cons(elems.head, apply(elems.tail: _*))

  // Exercise 3.2
  def tail[A](l: List[A]) = l match {
    case Cons(_, tail) => Some(tail)
    case Nil => None
  }

  def setHead[A](list: List[A], elem: A) = list match {
    case Cons(_, tail) => Cons(elem, tail)
    case Nil => Cons(elem, Nil)
  }

  def drop[A](list: List[A], n: Int) = {
    @tailrec
    def dropRec(current: List[A], left: Int): List[A] = (current, left) match {
      case (Nil, _) => Nil
      case (cons, 0) => cons
      case (Cons(_, tail), n) if n > 0 => dropRec(tail, n - 1)
    }
    dropRec(list, n)
  }

  /**
   * Make possible for our lists to retrieve the product of its members
   *
   * @param list the list of elements to take the product
   * @tparam A the type parameter bounded to Numeric
   * @return An A representing the product
   */
  def product[A : Numeric](list: List[A]): A = {
    val numeric = implicitly[Numeric[A]]
    @annotation.tailrec
    def productRec(acum: A, left: List[A]): A = {
      left match {
        case Nil => acum
        case Cons(head, tail) => productRec(numeric.times(head, acum), tail)
      }
    }
    productRec(numeric.one, list)
  }

  /**
   * Make possible for our lists to retrieve the sum of its members
   *
   * @param list the list of elements to take the sum
   * @tparam A the type parameter bounded to Numeric
   * @return An A representing the sum
   */
  def sum[A : Numeric](list: List[A]): A = {
    val numeric = implicitly[Numeric[A]]
    @annotation.tailrec
    def sumRec(acum: A, left: List[A]): A = {
      left match {
        case Nil => acum
        case Cons(head, tail) => sumRec(numeric.plus(head, acum), tail)
      }
    }
    sumRec(numeric.zero, list)
  }

  /**
   * This would be the equivalent of (Num a) => [a] -> (a -> a -> a) -> a -> a
   *
   * @param list the list of As
   * @param f the function to apply to acum and head (BE AWARE of the order)
   * @param zero the zero element of the function
   * @tparam A the type of the list
   * @return an A produced by subsequently applying the function
   */
  def op[A : Numeric](list: List[A], f: (A, A) => A, zero: A): A = {
    @annotation.tailrec
    def opRec(acum: A, left: List[A]): A = {
      left match {
        case Nil => acum
        case Cons(head, tail) => opRec(f(acum, head), tail)
      }
    }
    opRec(zero, list)
  }

}
