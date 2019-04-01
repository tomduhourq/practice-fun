

val listFail = List(1,2,3,9)
val listSuccess = List(1,2,4,4)
val sum = 8

def findMatchingPair(numbers: List[Int], sum: Int): Option[(Int, Int)] =
{
  var complements: Set[Int] = Set()
  for (number <- numbers) {
    if(complements.contains(sum - number))
      return Some((sum - number, number))
    else
      complements = complements + number
  }
  None
}

findMatchingPair(listFail, sum)
findMatchingPair(listSuccess, sum)