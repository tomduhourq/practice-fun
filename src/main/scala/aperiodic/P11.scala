package aperiodic

object P11 extends P10[Any] with App {
  def encodedModified(list: List[Any]): List[Any] = encode(list) map {
    case (1, element) => element
    case tuple        => tuple
  }

  assert(encodedModified(List()) == List())
  assert(encodedModified(List('a, 'b, 'a)) == List('a, 'b, 'a))
  assert(encodedModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
}
