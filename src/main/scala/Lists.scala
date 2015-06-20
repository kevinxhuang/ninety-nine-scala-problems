/**
 * Solutions to P01-P28
 */
object Lists {

	//P01 (*) Find the last element of a list.
	//	Example:
	//		scala> last(List(1, 1, 2, 3, 5, 8))
	//	res0: Int = 8
	def last[T](list : List[T]): T = {
		list.last
	}

	//P02 (*) Find the last but one element of a list.
	//Example:
	//	scala> penultimate(List(1, 1, 2, 3, 5, 8))
	//res0: Int = 5
	def penultimate[T](list : List[T]): T = {
		if (list.length >= 2) {
			list(list.length - 2)
		} else {
			throw new RuntimeException("Elements of the list should not less than two.")
		}
	}

	//P03 (*) Find the Kth element of a list.
	//By convention, the first element in the list is element 0.
	//Example:
	//
	//	scala> nth(2, List(1, 1, 2, 3, 5, 8))
	//res0: Int = 2
	def nth[T](list: List[T], n: Int): T = {
		list(n - 1)
	}

	//P04 (*) Find the number of elements of a list.
	//	Example:
	//	scala> length(List(1, 1, 2, 3, 5, 8))
	//res0: Int = 6
	def length[T](list: List[T]): Int = {
		list.length
	}

	//P05 (*) Reverse a list.
	//Example:
	//	scala> reverse(List(1, 1, 2, 3, 5, 8))
	//res0: List[Int] = List(8, 5, 3, 2, 1, 1)
	def reverse[T](list: List[T]): List[T] = {
		list.reverse
	}

	//P06 (*) Find out whether a list is a palindrome.
	//	Example:
	//	scala> isPalindrome(List(1, 2, 3, 2, 1))
	//res0: Boolean = true
	def isPalindrome[T](list: List[T]): Boolean = {
		list == list.reverse
	}

	//P07 (**) Flatten a nested list structure.
	//Example:
	//	scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
	//res0: List[Any] = List(1, 1, 2, 3, 5, 8)
	def flatten(list: List[Any]): List[Any] = {
		list.flatMap({
			case l: List[_] => flatten(l)
			case e => List(e)
		})
	}

	//P08 (**) Eliminate consecutive duplicates of list elements.
	//	If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
	//	Example:
	//
	//	scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
	def compress[T](list: List[T]): List[T] = {
		list match {
			case Nil => Nil
			case head :: tailList => head :: compress(tailList.dropWhile(_ == head))
		}
	}

	//P09 (**) Pack consecutive duplicates of list elements into sublists.
	//	If a list contains repeated elements they should be placed in separate sublists.
	//	Example:
	//
	//	scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
	def pack[T](list: List[T]): List[List[T]] = {
		if (list.isEmpty) {
			List(List())
		} else {
			val (packed, tail) = list.span(_ == list.head)
			if (tail == Nil) {
				List(packed)
			} else {
				packed::pack(tail)
			}
		}
	}

	//P10 (*) Run-length encoding of a list.
	//	Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
	//Example:
	//
	//	scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	def encode[T](list: List[T]): List[(Int, T)] = {
		val packed = pack(list)
		packed.map(e => (e.length, e.head))
	}

	//P11 (*) Modified run-length encoding.
	//Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
	//Example:
	//
	//	scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
	def encodeModified[T](list: List[T]): List[Any] = {
		encode(list).map(item => if (item._1 == 1) item._2 else item)
	}

	//P12 (**) Decode a run-length encoded list.
	//	Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
	//Example:
	//
	//	scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	//res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	def decode[T](list: List[(Int, T)]): List[T] = {
		list.flatMap(e => List.fill(e._1)(e._2))
	}

	//P13 (**) Run-length encoding of a list (direct solution).
	//	Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
	//Example:
	//
	//	scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	def encodeDirect[T](list: List[T]): List[(Int, T)] = {
		if (list.isEmpty) {
			Nil
		} else {
			val (packed, next) = list.span(_ == list.head)
			(packed.length, packed.head)::encodeDirect(next)
		}
	}

	//P14 (*) Duplicate the elements of a list.
	//	Example:
	//	scala> duplicate(List('a, 'b, 'c, 'c, 'd))
	//res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
	def duplicate[T](list: List[T]): List[T] = {
		list.flatMap(e => List(e, e))
	}

	//P15 (**) Duplicate the elements of a list a given number of times.
	//Example:
	//	scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
	//res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
	def duplicateN[T](n: Int, list: List[T]): List[T] = {
		list.flatMap(e => List.fill(n)(e))
	}

	//P16 (**) Drop every Nth element from a list.
	//Example:
	//	scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	//res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
	def drop[T](n: Int, list: List[T]): List[T] = {
		list.zipWithIndex.filter(e => (e._2 + 1) % n != 0).flatMap(e => List(e._1))
	}

	//	P17 (*) Split a list into two parts.
	//		The length of the first part is given. Use a Tuple for your result.
	//	Example:
	//
	//		scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	//	res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	def split[T](n:Int, list: List[T]): (List[T], List[T]) = {
		list.splitAt(n)
	}

}
