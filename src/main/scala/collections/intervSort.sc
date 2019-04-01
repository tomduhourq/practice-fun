import scala.annotation.tailrec
import scala.util.{Random, Sorting}
def time[A](block: => A): A = {
  val start = System.currentTimeMillis()
  val f = block
  println(s"Elapsed: ${(System.currentTimeMillis() - start)/1e6}ms.")
  f
}


// O(n logn) a todos le aplica una pasada que va reduciendo el universo
// O(nˆ2) peor caso
def quicksort(l: List[Int]): List[Int] = {
  if(l.length <= 1) l
  else {
    val midElem = l(l.length / 2)
    List.concat(
      quicksort(l filter (midElem > _)),
      l filter (midElem == _),
      quicksort(l filter (midElem < _)))
  }
}


val randomArray = List.fill(4000)(Random.nextInt(2300))
val ordered = List.fill(4000)(23)
time(quicksort(randomArray))
time(quicksort(ordered))
quicksort(List(2,3,4,1,1,1))


// Insertion sort:
// if list is empty Nil
// else insert(head, sort(tail))
// insert head in order after sorting tail
def insert(head: Int, tail: List[Int]): List[Int] =
  tail match {
    case Nil => List(head)
    case l2 @ head2 :: tail2 =>
      if(head <= head2) head :: l2
      else head2 :: insert(head, tail2)
  }

def insertionSort(l: List[Int]): List[Int] =
  l match {
    case Nil => Nil
    case head :: tail => insert(head, insertionSort(tail))
  }

time(insertionSort(randomArray))
time(insertionSort(ordered))


// Merge sort
// empty? => list
// else merge(sort(take half), sort(drop half)

def mergeSort(l: List[Int]): List[Int] = {
  def mergeRec(left: List[Int], right: List[Int]): List[Int] = {
    if(left.isEmpty) right
    else if(right.isEmpty) left
    else if(left.head <= right.head) left.head :: mergeRec(left.tail, right)
    else right.head :: mergeRec(left, right.tail)
  }
  val half = l.length / 2
  if(half == 0) l
  else {
    mergeRec(mergeSort(l take half), mergeSort(l drop half))
  }
}

mergeSort(randomArray)


// Bubble
def bubbleSort(l: List[Int]): List[Int] = {
  def sort(acumNotSorted: List[Int], acumSorted: List[Int]): List[Int] =
    if (acumNotSorted.isEmpty) acumSorted
    else bubble(acumNotSorted, Nil, acumSorted)

  // sort a single element
  def bubble(left: List[Int], acumNotSorted: List[Int], acumSorted: List[Int]): List[Int] =
    left match {
    case x :: y :: xs =>
      if (x <= y) bubble(y :: xs, x :: acumNotSorted, acumSorted)
      else bubble(x :: xs, y :: acumNotSorted, acumSorted)
    case x :: Nil => sort(acumNotSorted, x :: acumSorted)
  }

  sort(l, Nil)
}

bubbleSort(List(5,4,1,2))


// Counting Sort O(n)
def countSort(l: Array[Int])= {
  val min = l.min
  val max = l.max
  l.foldLeft(Array.fill(max - min + 1)(0)) {(arr, n) =>
    arr(n - min) += 1
    arr
  }
  .zipWithIndex
  .collect { case (quant, elem) if quant != 0 => (quant, elem + min)}
  .flatMap{ case (quant, elem) => Array.fill(quant)(elem)}
}

countSort(Array(1,5,67,1,3,5,1,45,7,61,2,5,869))



// Performs TimSort or legacy merge sort if requested
Array(1,2,3,1,5,0).sorted

// Best algo to search in ordered array is binary search
// Complexity O(log n) on each step it reduces the universe to search by half
def search(target:Int, l:List[Int]): Option[Int] = {
  @tailrec
  def recursion(lo: Int, hi: Int):Option[Int] =
    if(hi < lo) None
    else {
      val mid = (lo + hi) / 2
      if (l(mid) > target) recursion(lo, mid - 1)
      else if (l(mid) < target) recursion(mid + 1, hi)
      else Some(mid)
    }


  recursion(lo = 0, hi = l.length - 1)
}

