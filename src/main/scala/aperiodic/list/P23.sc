// Extract a given number of randomly selected numbers from a List
def randomSelect[A](n: Int, l: List[A]) = {
  val size = l.length
  (1 to n)
    .map { x =>
    val pos = scala.util.Random.nextInt(size - 1)
    l(pos)
  }.toList
}
