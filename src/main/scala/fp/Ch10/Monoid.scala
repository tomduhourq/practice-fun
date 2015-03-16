package fp.Ch10

/**
 * Laws of associativity and identity hold.
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  val zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String]{
    def op(a1: String, a2: String) = a1 + a2
    val zero = " "
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 1 p.177 --> implement several monoids
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  // Exercise 2 p.178 --> Monoid for Option
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
    val zero = None
  }

  // Exercise 3 p.178 --> Monoid for endofunctions
  def EndoMonoid[A] = new Monoid[A => A] {
    // First apply a2 and then a1
    def op(a1: A => A, a2: A => A) = a1 compose a2
    // Our zero here would be the identity
    val zero = (a:A) => a
  }

  // Exercise 5 p.180 --> Monoid to insert spaces
  val wordsMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1.trim + " " + a2
    val zero = " "
  }
}
