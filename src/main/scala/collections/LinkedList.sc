sealed trait LinkedList{
  var next: LinkedList
}
case class Node[A](value: A, var next: LinkedList) extends LinkedList


val example = Node(2, Node(3, Node(4, null)))

def reverse2[A](head: LinkedList): LinkedList = {
  var prev: LinkedList = null
  var curr: LinkedList = head
  var foll: LinkedList = head

  while(curr != null) {
    foll = foll.next
    curr.next = prev
    prev = curr
    curr = foll
  }
  prev
}

// Always prev behind curr + foll
// curr != null
// foll = foll.next
// curr.next = prev
// prev = curr
// curr = foll


reverse2(example)