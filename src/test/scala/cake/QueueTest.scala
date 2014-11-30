package cake
import org.scalatest._
class QueueTest extends FunSuite {

  test("Enqueue elements 1,2,3, dequeue and receive 1") {
    var queue: Queue = new Queue
    queue enqueue (1) enqueue (2) enqueue (3)
    assert(queue.dequeue == 1)
  }
}