package fp

import fp.Ch3.{Leaf, Tree, Branch}
import org.scalatest.FlatSpec

class Ch3Test extends FlatSpec {


  val secondLevel = Branch(Leaf(6), Leaf(9))
  val firstLevel = Branch(Branch(Leaf(3), secondLevel), Leaf(7))
  val tree = Branch(Leaf(2), firstLevel)

  behavior of "orders"
    it should "prefix" in {
      assert(Tree.prefix(tree) == "23697")
    }

}
