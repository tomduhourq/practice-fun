package performance

/**
 * Created by tomas on 22/03/15.
 */
object Perf {
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: " + (System.nanoTime - s)/1e6 + "ms")
    ret
  }
}
