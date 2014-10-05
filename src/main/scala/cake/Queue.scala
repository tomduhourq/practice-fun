package cake

import scala.collection.immutable.Stack

class Queue {
  
	var stack1: Stack[Any] = new Stack[Any]()
	var stack2: Stack[Any] = new Stack[Any]()
	//--The idea is to have a queue implemented with two stacks
	def isEmpty = stack1.isEmpty
	def enqueue(elem: Any) = {
	  while(!stack2.isEmpty)
	    stack1.push(stack2.pop)
	  stack1.push(elem)
	}
	def dequeue = {
	  while(!stack1.isEmpty)
	    stack2.push(stack1.pop)
	  stack2.pop
	}
	
}