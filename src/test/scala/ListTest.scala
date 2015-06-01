import org.testng.annotations.Test
import org.testng.Assert._

class ListTest {

	@Test
	def testP01: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(P01.last(list), 8)
	}

	@Test
	def testP02: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(P02.penultimate(list), 5)
	}

	@Test
	def testP03: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(P03.nth(list, 2), 1)
	}

	@Test
	def testP04: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(P04.length(list), 6)
	}

	@Test
	def testP05: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(P05.reverse(list), List(8, 5, 3, 2, 1, 1))
	}

	@Test
	def testP06: Unit = {
		val list1 = List(1, 2, 3, 2, 1)
		val list2 = List(1, 2, 3, 2, 0)
		assertEquals(P06.isPalindrome(list1), true)
		assertEquals(P06.isPalindrome(list2), false)
	}

	@Test
	def testP07: Unit = {
		val list = List(List(1, 1), 2, List(3, List(5, 8)))
		assertEquals(P07.flatten(list), List(1, 1, 2, 3, 5, 8))
	}

	@Test
	def testP08: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(P08.compress(list), List('a, 'b, 'c, 'a, 'd, 'e))
	}
}
