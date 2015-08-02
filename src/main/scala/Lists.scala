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

	//P09 (**) Pack consecutive duplicates of list elements into sub
	//	If a list contains repeated elements they should be placed in separate sub
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

	//	P18 (**) Extract a slice from a list.
	//		Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
	//	Example:
	//
	//		scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	//	res0: List[Symbol] = List('d, 'e, 'f, 'g)
	def slice[T](start: Int, end: Int, list: List[T]): List[T] = {
		list.splitAt(start)._2.take(end - start)
	}

	//	P19 (**) Rotate a list N places to the left.
	//		Examples:
	//		scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	//	res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
	//
	//	scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	//	res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
	def rotate[T](n: Int, list: List[T]): List[T] = {
		if (n >= 0) {
			list.splitAt(n)._2 ::: list.splitAt(n)._1
		} else {
			val offset = -n
			list.splitAt(list.length - offset)._2 ::: list.splitAt(list.length - offset)._1
		}
	}

	//	P20 (*) Remove the Kth element from a list.
	//	the list and the removed element in a Tuple. Elements are numbered from 0.
	//	Example:
	//
	//		scala> removeAt(1, List('a, 'b, 'c, 'd))
	//	res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
	def removeAt[T](n: Int, list: List[T]): (List[T], T) = {
		if (n < 0 || n >= list.length) {
			throw new IllegalArgumentException
		} else {
			list.splitAt(n) match {
				case(pre, e::post) => (pre:::post, e)
			}
		}
	}

	//	P21 (*) Insert an element at a given position into a list.
	//		Example:
	//		scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
	//	res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
	def insertAt[T](elem: T, n: Int, list: List[T]): List[T] = {
		list.splitAt(n)._1 ::: elem :: list.splitAt(n)._2
	}

	//	P22 (*) Create a list containing all integers within a given range.
	//		Example:
	//		scala> range(4, 9)
	//	res0: List[Int] = List(4, 5, 6, 7, 8, 9)
	def range(start: Int, end: Int): List[Int] = {
		(4 to 9).toList
	}

	//	P23 (**) Extract a given number of randomly selected elements from a list.
	//	Example:
	//		scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
	//	res0: List[Symbol] = List('e, 'd, 'a)
	def randomSelect[T](n: Int, list: List[T]): List[T] = {
		if (n <= 0) {
			Nil
		} else {
			val (rest, e) = removeAt(util.Random.nextInt(list.length), list)
			e :: randomSelect(n - 1, rest)
		}
	}

	//	P24 (*) Lotto: Draw N different random numbers from the set 1..M.
	//		Example:
	//		scala> lotto(6, 49)
	//	res0: List[Int] = List(23, 1, 17, 33, 21, 37)
	def lotto(n: Int, m: Int): List[Int] = {
		randomSelect(n, range(1, m))
	}

	//	P25 (*) Generate a random permutation of the elements of a list.
	//		Hint: Use the solution of problem P23.
	//	Example:
	//
	//		scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
	//	res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
	def randomPermute[T](list : List[T]): List[T] = {
		util.Random.shuffle(list)
	}

	//	P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
	//	In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
	//		Example:
	//
	//		scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
	//	res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
	def combinations[T](n: Int, list: List[T]): List[List[T]] = {
		list match {
			case Nil => Nil
			case head:: rest =>
				if (n < 0 || n > list.length) {
					Nil
				} else if (n == 1) {
					list.map(List(_))
				} else {
					combinations(n - 1, rest).map(head :: _) ::: combinations(n, rest)
				}
		}
	}

	//	P27 (**) Group the elements of a set into disjoint subsets.
	//	a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
	//		Example:
	//
	//		scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
	//	res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
	//	b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will a list of groups.
	//
	//	Example:
	//
	//		scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
	//	res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
	//	Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
	//
	//	You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
	def group[T](counts: List[Int], list: List[T]): List[List[List[T]]] = {
		counts match {
			case Nil => List(Nil)
			case n :: restCounts => combinations(n, list).flatMap(c => group(restCounts, list.filter(e => !c.contains(e))).map(c :: _))
		}
	}

	def group3[T](list: List[T]): List[List[List[T]]] = {
		group(List(2, 3, 4), list)
	}

	//	P28 (**) Sorting a list of lists according to length of sublists.
	//		a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
	//		Example:
	//
	//		scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
	//	res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
	//	b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
	//
	//	Example:
	//
	//		scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
	//	res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
	//	Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
	def lsort[T](list: List[List[T]]): List[List[T]] = {
		list.sortBy(e => e.length)
	}

	def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
		val freqs = list.groupBy(e => e.length).map(e => e._1 -> e._2.length)
		list.sortBy(e => (freqs.get(e.length), -e.length))
	}
}
