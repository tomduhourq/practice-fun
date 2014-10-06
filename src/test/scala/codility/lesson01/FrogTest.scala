package codility.lesson01
import org.junit._
import org.scalatest.junit.AssertionsForJUnit
class FrogTest {
	@Test
	def Jumps3Test(){
	  Assert.assertEquals(new Frog().minimumJumps(10, 85, 30),3)
	}
	
	@Test
	def Jumps4Test(){
	  Assert.assertEquals(new Frog().minimumJumps(10, 30, 2),10)
	}
}