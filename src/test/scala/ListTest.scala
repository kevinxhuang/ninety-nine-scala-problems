import org.testng.annotations.Test
import org.testng.Assert._

class ListTest {

	@Test
	def testP01: Unit = {
		assertEquals(
			Lists.last(List(1, 1, 2, 3, 5, 8)), 8)
	}

	@Test
	def testP02: Unit = {
		assertEquals(Lists.penultimate(List(1, 1, 2, 3, 5, 8)), 5)
	}

	@Test
	def testP03: Unit = {
		assertEquals(Lists.nth(List(1, 1, 2, 3, 5, 8), 2), 1)
	}

	@Test
	def testP04: Unit = {
		assertEquals(Lists.length(List(1, 1, 2, 3, 5, 8)), 6)
	}

	@Test
	def testP05: Unit = {
		assertEquals(Lists.reverse(List(1, 1, 2, 3, 5, 8)), List(8, 5, 3, 2, 1, 1))
	}

	@Test
	def testP06: Unit = {
		assertEquals(Lists.isPalindrome(List(1, 2, 3, 2, 1)), true)
		assertEquals(Lists.isPalindrome(List(1, 2, 3, 2, 0)), false)
	}

	@Test
	def testP07: Unit = {
		assertEquals(Lists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))), List(1, 1, 2, 3, 5, 8))
	}

	@Test
	def testP08: Unit = {
		assertEquals(Lists.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)),
			List('a, 'b, 'c, 'a, 'd, 'e))
	}

	@Test
	def testP09: Unit = {
		assertEquals(Lists.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)),
			List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
	}

	@Test
	def testP10: Unit = {
		val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		assertEquals(Lists.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)),
			List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
	}

	@Test
	def testP11: Unit = {
		assertEquals(Lists.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)),
			List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
	}

	@Test
	def testP12: Unit = {
		assertEquals(Lists.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))),
			List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	}

	@Test
	def testP13: Unit = {
		assertEquals(Lists.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)),
			List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
	}

	@Test
	def testP14: Unit = {
		assertEquals(Lists.duplicate(List('a, 'b, 'c, 'c, 'd)),
			List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
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

	@Test
	def testP18: Unit = {
		assertEquals(Lists.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
			List('d, 'e, 'f, 'g))
	}

	@Test
	def testP19: Unit = {
		assertEquals(Lists.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
			List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
		assertEquals(Lists.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
			List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
	}

	@Test
	def testP20: Unit = {
		assertEquals(Lists.removeAt(1, List('a, 'b, 'c, 'd)),
			(List('a, 'c, 'd),'b))
	}

	@Test
	def testP21: Unit = {
		assertEquals(Lists.insertAt('new, 1, List('a, 'b, 'c, 'd)),
			List('a, 'new, 'b, 'c, 'd))
	}

	@Test
	def testP22: Unit = {
		assertEquals(Lists.range(4, 9),
			List(4, 5, 6, 7, 8, 9))
	}

	@Test
	def testP23: Unit = {
		assertEquals(Lists.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).size, 3)
	}

	@Test
	def testP24: Unit = {
		assertEquals(Lists.lotto(6, 49).size, 6)
		assertEquals(Lists.lotto(6, 49).filter(e => e <= 49).size, 6)
	}

	@Test
	def testP25: Unit = {
		assertEquals(Lists.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)).canEqual(List('a, 'b, 'c, 'd, 'e, 'f)), true)
		assertEquals(Lists.randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)).size, List('a, 'b, 'c, 'd, 'e, 'f).size)
	}

	@Test
	def testP26: Unit = {
		assertEquals(Lists.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).size, 20)
	}

	@Test
	def testP27: Unit = {
		assertEquals(Lists.group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).size, 1260)
		assertEquals(Lists.group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")).size, 756)
	}

	@Test
	def testP28: Unit = {
		assertEquals(Lists.lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))),
			List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
		assertEquals(Lists.lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))),
			List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
	}
}
