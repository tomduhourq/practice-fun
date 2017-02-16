package aperiodic

// Usage of equivalence classes between `places` and list.length
class P19[A] {

  def rotate(places: Int, list: List[A]): List[A] = {
    val length = list.length
    val absValue = abs(places)
    val realPlaces = absValue % length

    if (realPlaces == 0) list
    else equivalenceRotation(places, list, absValue, realPlaces, length)
  }

  private def equivalenceRotation(places: Int, list: List[A], absValue: Int, realPlaces: Int, length: Int): List[A] = {
    if (absValue < length) directionRotation(places, length, list)
    else {
      if (places < 0) directionRotation(-realPlaces, length, list)
      else directionRotation(realPlaces, length, list)
    }
  }

  private def directionRotation(realPlaces: Int, length: Int, list: List[A]): List[A] = {
    if (realPlaces > 0) list.takeRight(realPlaces) ::: list.take(length - realPlaces)
    else list.takeRight(length + realPlaces) ::: list.take(-realPlaces)
  }

  private def abs(x: Int) = if (x < 0) -x else x
}
