package codility.lesson03

/**
 * Created by tomas on 03/03/15.
 */
object SuffixPreffixHelper {

  def preffix(a: Array[Int]): Array[Int] = {
    val (l, preffix) = prepare(a)
    for (i <- Range(0, l))
      preffix(i + 1) = preffix(i) + a(i)
    preffix
  }

  def suffix(a: Array[Int]): Array[Int] = {
    val (l, suffix) = prepare(a)
    for(i <- Range(0,l).reverse)
        suffix(i) = a(i) + suffix(i + 1)
    suffix
  }

  def prepare(a: Array[Int]): (Int, Array[Int]) = {
    val l = a.length
    val suffix = Array.fill(l + 1)(0)
    (l, suffix)
  }
}