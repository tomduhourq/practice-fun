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

  // Exercise 4 p.40 --> dropWhile. Removes the elements from the List prefix as long as they match a predicate.
  // By passing the arguments one by one, we are returning a function that takes f separately in order to help the compiler
  // infer the types of f and not having to do something like (x: Int) => x > 45
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
}
