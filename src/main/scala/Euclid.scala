import scala.annotation.tailrec

/**
  * Euclidean and related algorithms for BigInts
  */
object Euclid {

  final private val Zero = BigInt(0)

  /**
    * Greatest common divisor
    */
  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = b match {
    case Zero => a
    case _ => gcd(b, a % b)
  }

  /**
    * Least common multiple
    */
  def lcm(a: BigInt, b: BigInt): BigInt = a * (b / gcd(a, b))

  /**
    * Bézout numbers via the extended Euclidean algorithm
    *
    * @return Tuple (r, s, t) such that r == a*s + b*t and r == gcd(a, b)
    */
  def bezout(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt) = {
    @tailrec
    def bezoutR(r0: BigInt=a, s0: BigInt=1, t0: BigInt=0,
                r1: BigInt=b, s1: BigInt=0, t1: BigInt=1):
    (BigInt, BigInt, BigInt) = r1 match {
      case Zero => (r0, s0, t0)
      case _ =>
        val q = r0 / r1
        bezoutR(r1, s1, t1, r0 % r1, s0 - q * s1, t0 - q * t1)
    }
    bezoutR()
  }

  /**
    * Modular division
    *
    * @return Some(m) where m*d == a(mod n), or None if no such m exists.
    */
  def modDiv(a: BigInt, n: BigInt, divisor: BigInt): Option[BigInt] =
  bezout(divisor, n) match {
    case (Zero, _, _) => None  // Avoid error (a % 0) in next line
    case (g, s, t) if a % g == 0 =>
      // Since a == m*g and Bézout numbers satisfy
      // the equality s*d + t*n == g, we find m*s*d == a (mod q)
      Option((a/g) * s)
    case _ => None // g does not divide c, so no divisor exists
  }

  /**
    * Continued fraction expansion of a rational number
    *
    * @param n Numerator
    * @param d Denominator
    * @return Stream of partial quotients a
    */
  def continuedFraction(n: BigInt, d: BigInt): Stream[BigInt] = (n, d) match {
    case (_, Zero) => Stream.empty
    case _ =>
      val a = n / d
      a #:: continuedFraction(d, n - d * a)
  }

  /**
    * Convergents of a rational number
    *
    * @param n Numerator
    * @param d Denominator
    * @return Stream of (p, q) defining convergents p / q
    */
  def convergents(n: BigInt, d: BigInt): Stream[(BigInt, BigInt)] = {
    def convergentsR(p: BigInt,
                     pp: BigInt,
                     cf: Stream[BigInt]): Stream[BigInt] = cf match {
      case Stream.Empty => p #:: Stream.Empty
      case h #:: tail =>
        val next = h * p + pp
        p #:: convergentsR(next, p, tail)
    }

    val cf = continuedFraction(n, d)
    val qStream = convergentsR(1, 0, cf.tail)
    val pStream = convergentsR(cf.head, 1, cf.tail)

    pStream.zip(qStream)
  }

  /**
    * Best rational approximation for n/d with denominator < m
    *
    * @note m should be greater than one
    * @return Pair (n', d') with d' < m
    */
  def rationalApprox(n: BigInt, d: BigInt, m: BigInt): (BigInt, BigInt) = {
    convergents(n, d).takeWhile(_._2 < m).last
  }
}
