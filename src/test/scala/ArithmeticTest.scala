import org.testng.Assert._
import org.testng.annotations.Test

class ArithmeticTest {

  @Test
  def testP31(): Unit = {
    assertEquals(Arithmetic.isPrime(1), false)
    assertEquals(Arithmetic.isPrime(2), true)
    assertEquals(Arithmetic.isPrime(4), false)
  }

  @Test
  def testP32(): Unit = {
    assertEquals(Arithmetic.gcd(36, 63), 9)
  }

  @Test
  def testP33(): Unit = {
    assertEquals(Arithmetic.isCoprimeTo(35, 64), true)
    assertEquals(Arithmetic.isCoprimeTo(36, 64), false)
  }

  @Test
  def testP34(): Unit = {
    assertEquals(Arithmetic.totient(10), 4)
  }

  @Test
  def testP35(): Unit = {
    assertEquals(Arithmetic.primeFactors(2), List(2))
    assertEquals(Arithmetic.primeFactors(315), List(3, 3, 5, 7))
  }

  @Test
  def testP36(): Unit = {
    assertEquals(Arithmetic.primeFactorMultiplicity(315), Map(3 -> 2, 5 -> 1, 7 -> 1))
  }

  @Test
  def testP37(): Unit = {
    assertEquals(Arithmetic.phi(315), 144)
  }

  @Test
  def testP39(): Unit = {
    assertEquals(Arithmetic.listPrimesinRange(7 to 31), List(7, 11, 13, 17, 19, 23, 29, 31))
  }

  @Test
  def testP40(): Unit = {
    assertEquals(Arithmetic.goldbach(28), (5, 23))
  }

  @Test
  def testP41(): Unit = {
    Arithmetic.printGoldbachListGoldbachList(9 to 20)
    Arithmetic.printGoldbachListLimited(1 to 2000, 50)
  }
}