package fp.Ch3

/**
 * Created by tomas on 22/02/15.
 */
class Lists {

  // Exercise 2 p.40 --> tail. Retrieve the tail from a List.
  def tail[A](list: List[A]): List[Any] = list match {
    case Nil => Nil
    case x :: xs => xs
  }

  // Exercise 3 p.40 --> drop. Remove the first n elements of a list.
  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case x :: xs if n > 0 => drop(xs, n - 1)
    case l@x :: xs if n == 0 => l
  }

  /**
   * Exercise 4 p.40 --> dropWhile. Removes the elements from the List prefix as long as they match a predicate.
   * By passing the arguments one by one, we are returning a function that takes f separately in order to help the
   * compiler infer the types of f and not having to do something like (x: Int) => x > 45
   */
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case x :: xs if f(x) => dropWhile(xs)(f)
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
    case x :: xs => x :: append(xs, a2)
  }

  // Exercise 6 p.42 --> init. Retrieve all the elements except the last one from a list.
  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

  // NTH --> foldRight. Perform a series of operations recursively on a List to get a final result.
  // Note that z is the seed of the operation represented by f, and is the last operand to be evaluated.
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  // Exercise 9 p.44 --> length. Count the elements of a List using foldRight.
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, acum) => 1 + acum)

  // Exercise 10 p.44 --> foldLeft. Note that in foldLeft the seed is the acummulator
  // and it's also tail-recursive.
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 11 p.44 --> sum,product and length using foldLeft.
  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acum, a) => acum + 1)

  // Exercise 12 p.45 --> reverse.
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((xs, ys) => ys :: xs)

  def reverse2[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((xs: A, ys: List[A]) => ys ::: List(xs))

  // NTH --> reverse: compute a List in a reversed way
  def reverse3[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs => reverse(xs) :+ x
  }

  // Exercise 13 p.45 --> implement foldRight in terms of foldLeft and viceversa.
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // Exercise 14 p.45 --> implement append in terms of foldLeft or foldRight
  def appendLeft[A, B](l1: List[A], l2: List[A]): List[A] = foldLeft(l2, l1)((b, a) => b ::: List(a))

  def appendRight[A, B](l1: List[A], l2: List[A]): List[A] = foldRight(l2, l1)((a, b) => b ::: List(a))

  // Exercise 15 p.45 --> flatten. Concatenate a list of lists into a single list (trying to use things we have done).
  def flatten[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(appendLeft)

  def flattenInverse[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(appendRight)
  
  def flattenEasy[A](l: List[List[A]]): List[A] = for(xs <- l; x <- xs) yield x

  // Exercise 16 p.45 --> addOne. Adds 1 to each element of a List.
  def addOne[A](l: List[Int]): List[Int] = l.map(_ + 1)

  // Exercise 17 p.46 --> convToString: Convert a List[Double] into List[String]
  def convToString(l: List[Double]): List[String] = l map (_.toString)

  // Exercise 18 p.46 --> map: apply a function to each element of a List and return a new one.
  def map[A, B](l: List[A])(f: A => B): List[B] = foldLeft(l, Nil: List[B])((b, a) => b ::: List(f(a)))

  // Exercise 19. p.46 --> filter: removes the elements from a List unless they satisfy the given function.
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs if f(x) => x :: filter(xs)(f)
    case x :: xs if !f(x) => filter(xs)(f)
  }

  // Exercise 20 p.46 --> flatMap : map all elements of a List to Lists and flatten it
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  // Exercise 21 p.46 --> filter2: implement filter in terms of flatMap
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  // Exercise 22 p.46 --> addLists: add the elements of both lists.
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1.length == l2.length) {
      l1 match {
        case Nil => Nil
        case x :: xs => {
          val pos = l1.indexOf(x)
          x + l2(pos) :: addLists(xs, l2.drop(1))
        }
      }
    }
    else Nil
  }

  // Exercise 23 p.46 --> generalizeAction: the last function is called zipWith in Scala's List API.
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
  }

  // Exercise 24 p.47 --> hasSubsequence: determine if a List contains a given subsequence.
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    // This is the cut condition
    case (x :: xs, Nil) => true
    case (x :: xs, y :: ys) if x == y => hasSubsequence(xs, ys)
    case (x :: xs, y :: ys) if x != y => hasSubsequence(xs, sub)
  }
}
