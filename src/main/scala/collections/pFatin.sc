def time[A](f: => A) = {
  val s = System.nanoTime
  val ret = f
  println("time: " + (System.nanoTime - s)/1e6 + "ms")
  ret
}

// https://pavelfatin.com/scala-collections-tips-and-tricks/

/** References
  seq — an instance of Seq-based collection, like Seq(1, 2, 3)
  set — an instance of Set, for example Set(1, 2, 3)
  array — an array, e. g. Array(1, 2, 3)
  option — an instance of Option, for example, Some(1)
  map — an instance of Map, like Map(1 -> "foo", 2 -> "bar")
  p — a predicate function with type T => Boolean, like _ > 2
  n — an integer value
  i — an integer index
  f, g — simple functions, A => B
  x, y — some arbitrary values
  z — initial or default value
  */

val seq = Seq(1,2,3)
val set = Set(1,1,2,3)
val x = 1

// Getting to exists
seq.filter(_ == x).headOption != None
seq.find(_ == x) != None
seq.find(_ == x).isDefined
seq.exists(_ == x)
seq.contains(x)

// Functions that always return the same result to the same parameters are said to be
// referentially transparent or pure, while the ones having side effect aren't.
val ml = new StringBuilder("Tom")
// Isolation of side-effect collection-based expressions
seq.foreach(ml.append("ás"))
seq.find(_ > 1)

/** These examples are shown first the bad practice and then the good one **/

// ----------------------------------------------------------------------------------
// -- CREATION
// ----------------------------------------------------------------------------------

// Explicit creation is best
Seq[Int]()
Set[Int]()
Map[String, Int]()
Iterator[Int]()

Seq.empty[Int]
Set.empty[Int]
Option.empty[Int]
Map.empty[String, Int]
Iterator.empty

// ----------------------------------------------------------------------------------
// -- LENGTH
// ----------------------------------------------------------------------------------


// Prefer length to size in arrays, since size is computed via an implicit wrapper
time(Array(1, 2, 3).length)
time(Array(1, 2, 3).size)
// Don't negate emptiness-related properties
!seq.nonEmpty
!seq.isEmpty
seq.isEmpty
seq.nonEmpty
// Don't compute length for emptiness-check
seq.length > 2
seq.length != 2
seq.lengthCompare(2) > 0
seq.lengthCompare(2) != 0
// --------------------------------------------------------------------------------
// -- EQUALITY
// --------------------------------------------------------------------------------

// Don't rely on == to compare array contents (also applicable to Iterator

Array(1, 2, 3) == Array(1, 2, 3)
Array(1, 2, 3).sameElements(Array(1, 2, 3))
// Don't check equality between different category collections
seq == set
seq.toSet == set
// --------------------------------------------------------------------------------
// -- INDEXING
// --------------------------------------------------------------------------------

// Never retrieve first or last element by index
time(seq(0))
time(seq.head)
seq(seq.length - 1)
seq.last
// Don't check index bounds explicitly
if (2 < seq.length) Some(seq(2)) else None
seq.lift(2)
// Don't construct indices ranges manually
time(Range(0, seq.length))
time(seq.indices)
// ----------------------------------------------------------------------------------
// -- EXISTENCE
// ----------------------------------------------------------------------------------
// Don't use equality predicate to check element presence
time(seq.exists(_ == 1))
time(seq.contains(1))

// Don't use inequality predicate to check element absence
time(seq.forall(_ != 1))
time(!seq.contains(1))
// ----------------------------------------------------------------------------------
// -- FILTERING
// ----------------------------------------------------------------------------------

// Don't negate filter predicates
seq.filter(_ != 1)
seq.filterNot(_ == 1)
// Don't resort to filtering to count elements
seq.filter(_ == 1).length
seq.count(_ == 1)

// ----------------------------------------------------------------------------------
// -- SORTING
// ----------------------------------------------------------------------------------
// Don't sort by a property manually
val m = List(("two", 2),("one", 1), ("three", 3))

m.sortWith(_._2 < _._2)
m.sortBy(_._2)

val singles = m.map(_._2)

// Don't sort by identity manually
singles.sortBy(it => it)
singles.sortBy(identity)
singles.sortWith(_ < _)

singles.sorted
// Perform reverse sorting in one step
singles.sorted.reverse
singles.sortBy(identity).reverse
singles.sortWith(_ < _).reverse

singles.sorted(Ordering[Int].reverse)
singles.sortBy(identity)(Ordering[Int].reverse)
singles.sortWith((_ > _))





