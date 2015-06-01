//P02 (*) Find the last but one element of a list.
//Example:
//	scala> penultimate(List(1, 1, 2, 3, 5, 8))
//res0: Int = 5

object P02 {

	def penultimate[T](list : List[T]): T = {
		if (list.length >= 2) {
			list(list.length - 2)
		} else {
			throw new RuntimeException("Elements of the list should not less than two.")
		}
	}
}
