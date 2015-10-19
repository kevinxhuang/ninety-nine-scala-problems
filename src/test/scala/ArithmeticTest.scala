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

  def testP33(): Unit = {
    assertEquals(Arithmetic.gcd(35, 64), true)
    assertEquals(Arithmetic.gcd(36, 64), false)
  }

  def testP34(): Unit = {
    assertEquals(Arithmetic.totient(10), 4)
  }

  def testP35(): Unit = {
    assertEquals(Arithmetic.primeFactors(2), List(2))
    assertEquals(Arithmetic.primeFactors(315), List(3, 3, 5, 7))
  }

  def testP36(): Unit = {

  }

  def testP37(): Unit = {

  }

  def testP38(): Unit = {

  }

  def testP39(): Unit = {

  }

  def testP40(): Unit = {

  }

  def testP41(): Unit = {

  }
}