package trees

object tests {
  /* given a tree represented by a node called root, find the max value of the tree*/
  def findMax(root: Node): Int = {
  	def determineMax(actualNode: Node,actualMax: Int): Int = actualNode match {
  		//--Leaf
  		case Node(n,None,None) if n > actualMax => n
  		case Node(n,None,None) if n <= actualMax => actualMax
  		//--Only right Node exists
  		case Node(n,None,Some(right)) if n > actualMax =>  determineMax(right, n)
  		case Node(n,None,Some(right)) if n <= actualMax => determineMax(right,actualMax)
  		//--Only left Node exists
  		case Node(n,Some(left),None) if n > actualMax =>  determineMax(left, n)
  		case Node(n,Some(left),None) if n <= actualMax =>  determineMax(left, actualMax)
  		//--The two Nodes are present
  		case Node(n,Some(left),Some(right)) if n > actualMax =>
  						math.max(determineMax(left,n),determineMax(right, n))
  		case Node(n,Some(left),Some(right)) if n <= actualMax =>
  						math.max(determineMax(left,actualMax),determineMax(right, actualMax))
  	}
  	determineMax(root,-1)
  }                                               //> findMax: (root: trees.Node)Int
  
  findMax(Node(2,Some(Node(2,Some(Node(800,None,None)),None)),None))
                                                  //> res0: Int = 800
}