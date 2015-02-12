package aperiodic

/**
 * Created by tomasduhourq on 2/12/15.
 */
object P10encode {
  def encode(l: List[Any]): List[Any] = P09pack.pack(l).map{list => (list.length,list.head)}
}
