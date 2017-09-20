import org.junit.Test
import org.junit.Assert._

import Euclid._

class EuclidTest {

  @Test
  def gcdTest(): Unit = {
    assertEquals(BigInt(9), gcd(36, 63))
    assertEquals(BigInt(1), gcd(35, 64))
  }

  @Test
  def lcmTest(): Unit = {
   assertEquals(BigInt(36 * 63 / 9), lcm(36, 63))
   assertEquals(BigInt(101 * 103), lcm(101, 103))
  }

  @Test
  def bezoutTest(): Unit = {
    val a = 101 * 103
    val b = 103 * 107
    val (g, s, t) = bezout(a, b)

    assertEquals(BigInt(103), g)
    assertEquals(a * s + b * t, g)

  }

  @Test
  def bezoutRelativelyPrimeTest(): Unit = {
    val a = 101
    val b = 103
    val (g, s, t) = bezout(a, b)

    assertEquals(BigInt(1), g)
    assertEquals(a * s + b * t, g)

  }

  @Test
  def bezoutErrorTest(): Unit = {
    bezout(0, 1)
    bezout(1, 0)
    bezout(1, 1)
  }

  @Test
  def modularDivisionZeroDivisorTest(): Unit = {
    assertEquals((0, 1, 0), bezout(0, 0))
    assertEquals(None, modDiv(0, 0, 0))
  }

  @Test
  def continuedFractionTest(): Unit = {
    assertEquals(Stream[BigInt](0, 27), continuedFraction(1, 27))
    assertEquals(Stream[BigInt](1, 27), continuedFraction(28, 27))
    assertEquals(
      Stream[BigInt](1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 18, 1, 18),
      continuedFraction(BigInt("161803398875"), BigInt("100000000000"))
    )
  }

  @Test
  def convergentsTest(): Unit = {
    assertEquals(Stream((0, 1), (1, 27)), convergents(10, 270))
    assertEquals(Stream((1, 1), (28, 27)), convergents(280, 270))
    assertEquals(
      Stream((1,1), (2,1), (3,2), (5,3), (8,5), (13,8), (21,13), (34,21),
        (55,34), (89,55), (144,89), (233,144), (377,233), (610,377),
        (987,610), (1597,987), (2584,1597), (4181,2584), (6765,4181),
        (10946,6765), (17711,10946), (28657,17711), (46368,28657),
        (75025,46368), (121393,75025), (196418,121393), (317811,196418),
        (514229,317811), (832040,514229), (1346269,832040),
        (3524578,2178309), (64788673,40041602), (68313251,42219911),
        (1294427191,800000000)),
      convergents(BigInt("1618033988750"), BigInt("1000000000000")))
  }

  @Test
  def testRationalApprox(): Unit = {
    assertEquals((3, 1), rationalApprox(3141592, 1000000, 2))
    assertEquals((22, 7), rationalApprox(3141592, 1000000, 8))
    assertEquals((333, 106), rationalApprox(3141592, 1000000, 110))
    assertEquals((355, 113), rationalApprox(3141592, 1000000, 115))
  }

}