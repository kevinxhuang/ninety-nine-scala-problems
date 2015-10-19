/**
 * Solutions to P31-41
 */
object Arithmetic {
  //  P31 (**) Determine whether a given integer number is prime.
  //    scala> 7.isPrime
  //  res0: Boolean = true
  def isPrime(n : Int): Boolean = {
    if (n == 1) {
      false
    } else if (n == 2) {
      return true
    } else {
      return !(2 to math.sqrt(n.toDouble).toInt).exists(x => n % x == 0)
    }
  }

  //  P32 (**) Determine the greatest common divisor of two positive integer numbers.
  //    Use Euclid's algorithm.
  //  scala> gcd(36, 63)
  //  res0: Int = 9
  def gcd(a: Int, b: Int): Int ={
    if (a % b == 0) {
      return b
    } else {
      return gcd(b, a % b)
    }
  }

  //  P33 (*) Determine whether two positive integer numbers are coprime.
  //    Two numbers are coprime if their greatest common divisor equals 1.
  //  scala> 35.isCoprimeTo(64)
  //  res0: Boolean = true
  def isCoprimeTo(a: Int, b: Int): Boolean = {
    return gcd(a, b) == 1
  }

  //  P34 (**) Calculate Euler's totient function phi(m).
  //    Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
  //  scala> 10.totient
  //  res0: Int = 4
  def totient(n: Int): Int = {
    return (1 to n).count(x => isCoprimeTo(x, n))
  }

  //  P35 (**) Determine the prime factors of a given positive integer.
  //  Construct a flat list containing the prime factors in ascending order.
  //    scala> 315.primeFactors
  //  res0: List[Int] = List(3, 3, 5, 7)
  def primeFactors(n: Int): List[Int] = {
    def getFactors(n: Int, factors: Stream[Int]): List[Int] = {
      if (n == 1 || factors.isEmpty) {
        return List()
      }

      if (n % factors.head == 0) {
        return factors.head :: getFactors(n / factors.head, factors)
      } else {
        return getFactors(n, factors.tail)
      }
    }

    if (n < 2) {
      throw new IllegalArgumentException("Invalid input.")
    }
    if (isPrime(n)) {
      return List(n)
    } else {
      return getFactors(n, (2 to n).filter(isPrime).toStream)
    }
  }
}