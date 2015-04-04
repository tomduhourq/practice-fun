package performance


object Perf {
  // I needed f to be a call-by-name parameter since normal parametrization
  // would result in evaluation of f before I actually need it.
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: " + (System.nanoTime - s)/1e6 + "ms")
    ret
  }
}
