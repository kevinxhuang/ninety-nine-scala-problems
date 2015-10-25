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

  //  P36 (**) Determine the prime factors of a given positive integer (2).
  //    Construct a list containing the prime factors and their multiplicity.
  //  scala> 315.primeFactorMultiplicity
  //  res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
  //  Alternately, use a Map for the result.
  //
  //  scala> 315.primeFactorMultiplicity
  //  res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
  def primeFactorMultiplicity(n: Int): Map[Int, Int] = {
    var factorCount = Map[Int, Int]()
    primeFactors(n).foreach(f => factorCount += f -> (factorCount.getOrElse(f, 0) + 1))

    return factorCount
  }

  //  P37 (**) Calculate Euler's totient function phi(m) (improved).
  //    See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
  //    phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...
  //
  //  Note that ab stands for the bth power of a.
  def phi(m: Int): Int = {
    return primeFactorMultiplicity(m).foldLeft(1)({(x , y) => x * (y._1 - 1) * Math.pow(y._1, y._2 - 1).toInt})
  }

  //  P39 (*) A list of prime numbers.
  //  Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
  //  scala> listPrimesinRange(7 to 31)
  //  res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
  def listPrimesinRange(range: Range): List[Int] = {
    return range.toList.filter(isPrime _)
  }

  //  P40 (**) Goldbach's conjecture.
  //  Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
  //    scala> 28.goldbach
  //  res0: (Int, Int) = (5,23)
  def goldbach(n: Int): (Int, Int) = {
    val p = listPrimesinRange(2 to n).find(p => isPrime(n - p))
    return (p.get, n - p.get)
  }

  //  P41 (**) A list of Goldbach compositions.
  //  Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
  //    scala> printGoldbachList(9 to 20)
  //  10 = 3 + 7
  //  12 = 5 + 7
  //  14 = 3 + 11
  //  16 = 3 + 13
  //  18 = 5 + 13
  //  20 = 3 + 17
  //  In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.
  //
  //  Example (minimum value of 50 for the primes):
  //
  //  scala> printGoldbachListLimited(1 to 2000, 50)
  //  992 = 73 + 919
  //  1382 = 61 + 1321
  //  1856 = 67 + 1789
  //  1928 = 61 + 1867
  def printGoldbachListGoldbachList(range: Range): Unit = {
    range.foreach(x => {
      if (x % 2 == 0) {
        val g = goldbach(x)
        Console.out.println(x + " = " + g._1 + " + " + g._2)
      }
    })
  }

  def printGoldbachListLimited(range: Range, limit: Int): Unit = {
    range.filter(_ > limit).foreach(x => {
      if (x % 2 == 0) {
        val g = goldbach(x)
        Console.out.println(x + " = " + g._1 + " + " + g._2)
      }
    })
  }
}