package cci

class ListRotator[A](val list: List[A]) {

  private val rotateLeftFunction = (l: List[A], n: Int) => l.drop(n) ++ l.take(n)
  private val rotateRightFunction = (l: List[A], n: Int) => l.takeRight(n) ++ l.dropRight(n)

  private def validateAndApplyRotation(n: Int, f: (List[A], Int) => List[A]): Option[List[A]] = {
    if (list.length < n) None
    else                  Some(f(list, n))
  }

  def rotateLeft(n: Int): Option[List[A]] = validateAndApplyRotation(n, rotateLeftFunction)
  def rotateRight(n: Int): Option[List[A]] = validateAndApplyRotation(n, rotateRightFunction)
}

object ListRotator extends App {
  val rotator = new ListRotator(List(1, 2, 3, 4, 5))
  assert(rotator.rotateLeft(2).contains(List(3, 4, 5, 1, 2)))
  assert(rotator.rotateLeft(6).isEmpty)
  assert(rotator.rotateRight(2).contains(List(4, 5, 1, 2, 3)))
  assert(rotator.rotateRight(6).isEmpty)
}