package aperiodic


/**
 * Created by tomasduhourq on 2/12/15.
 */
object P11encodeModified {
  def encodeModified(l: List[Any]): List[Any] =
    P09pack.pack(l)
    .map{
      case l if l.length == 1 => l.head
      case l if l.length > 1 => (l.length,l.head)
    }
}
