import org.testng.annotations.Test
import org.testng.Assert._

class ListTest {

	@Test
	def testP01: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(Lists.last(list), 8)
	}

	@Test
	def testP02: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(Lists.penultimate(list), 5)
	}

	@Test
	def testP03: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(Lists.nth(list, 2), 1)
	}

	@Test
	def testP04: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(Lists.length(list), 6)
	}

	@Test
	def testP05: Unit = {
		val list = List(1, 1, 2, 3, 5, 8)
		assertEquals(Lists.reverse(list), List(8, 5, 3, 2, 1, 1))
	}

	@Test
	def testP06: Unit = {
		val list1 = List(1, 2, 3, 2, 1)
		val list2 = List(1, 2, 3, 2, 0)
		assertEquals(Lists.isPalindrome(list1), true)
		assertEquals(Lists.isPalindrome(list2), false)
	}

	@Test
	def testP07: Unit = {
		val list = List(List(1, 1), 2, List(3, List(5, 8)))
		assertEquals(Lists.flatten(list), List(1, 1, 2, 3, 5, 8))
	}

	@Test
	def testP08: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(Lists.compress(list), List('a, 'b, 'c, 'a, 'd, 'e))
	}

	@Test
	def testP09: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(Lists.pack(list), List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
	}

	@Test
	def testP10: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(Lists.encode(list), List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
	}

	@Test
	def testP11: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(Lists.encodeModified(list), List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
	}

	@Test
	def testP12: Unit = {
		val list = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
		assertEquals(Lists.decode(list), List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	}

	@Test
	def testP13: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(Lists.encodeDirect(list), List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
	}

	@Test
	def testP14: Unit = {
		val list = List('a, 'b, 'c, 'c, 'd)
		assertEquals(Lists.duplicate(list), List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
	}

	@Test
	def testP15: Unit = {
		assertEquals(Lists.duplicateN(3, List('a, 'b, 'c, 'c, 'd)),
			List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
	}

	@Test
	def testP16: Unit = {
		assertEquals(Lists.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
			List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
	}

	@Test
	def testP17: Unit = {
		assertEquals(Lists.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
			(List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
	}
}
