package fp

/**
 * Created by tomas on 11/02/15.
 */
class Ch3 {
  // Exercise 2 p.40 --> tail. Retrieve the tail from a List.
  def tail[A](list: List[A]): List[Any] = list match {
    case Nil => Nil
    case x :: xs => xs
  }

  // Exercise 3 p.40 --> drop. Remove the first n elements of a list.
  def drop[A](list: List[A], n: Int): List[A] = list match{
    case Nil => Nil
    case x :: xs if n > 0 => drop(xs, n-1)
    case l @ x :: xs if n == 0 => l
  }

  /** 
  * Exercise 4 p.40 --> dropWhile. Removes the elements from the List prefix as long as they match a predicate.
  * By passing the arguments one by one, we are returning a function that takes f separately in order to help the
  * compiler infer the types of f and not having to do something like (x: Int) => x > 45
  */
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case x :: xs if f(x) =>  dropWhile(xs)(f)
    case l @ x :: xs if !f(x) => l
  }

  // Exercise 5 p.41 --> setHead. Replace the first element of a List with a different value.
  def setHead[A](list: List[A], elem: A): List[A] = list match {
    case Nil => List(elem)
    case x :: xs => elem :: xs
  }

  // NTH --> append. Append listA to ListB. Its runtime is determined only by the length of a1.
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case x :: xs => x :: append(xs,a2)
  }

  // Exercise 6 p.42 --> init. Retrieve all the elements except the last one from a list.
  def init[A](list: List[A]): List[A] = list match{
    case Nil => Nil
    case x :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

  // NTH --> foldRight. Perform a series of operations recursively on a List to get a final result.
  // Note that z is the seed of the operation represented by f, and is the last operand to be evaluated.
  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs,z)(f))
  }

  // Exercise 9 p.44 --> length. Count the elements of a List using foldRight.
  def length[A](l: List[A]): Int = foldRight(l,0)((a,acum) => 1 + acum)

  // Exercise 10 p.44 --> foldLeft. Note that in foldLeft the seed is the acummulator
  // and it's also tail-recursive.
  def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft(xs,f(z,x))(f)
  }

  // Exercise 11 p.44 --> sum,product and length using foldLeft.
  def sum(l: List[Int]): Int = foldLeft(l,0)(_ + _)
  def product(l: List[Int]): Int = foldLeft(l,1)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l,0)((acum,a) => acum + 1)

  // Exercise 12 p.45 --> reverse.
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((xs, ys) => ys :: xs)
  def reverse2[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((xs: A , ys: List[A]) => ys ::: List(xs))

  // Exercise 13 p.45 --> implement foldRight in terms of foldLeft and viceversa.
  def foldLeft2[A,B](l: List[A], z: B)(f: (B,A) => B): B = foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
}
