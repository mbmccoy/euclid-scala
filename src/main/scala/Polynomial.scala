import Prime._
import algebra.ring.Field
import scala.language.implicitConversions
import scala.annotation.tailrec

/**
 * Polynomial in one variable over a field.
 */
case class Polynomial[T] private (private val coefficients: Map[Int, T])(implicit ev: Field[T]) {

  /**
   * Degree of the polynomial.
   */
  val degree: Int = (coefficients.keySet + 0).max

  /**
   * Get the coefficient for the exponent.
   */
  def get(degree: Int): T = coefficients.getOrElse(degree, ev.zero)

  /**
   * Add polynomials.
   */
  def + (that: Polynomial[T]): Polynomial[T] = Polynomial.apply(
    for (degree <- (this.coefficients.keySet ++ that.coefficients.keySet)) 
      yield (degree, ev.plus(this.get(degree), that.get(degree)))
  )

  /**
   * Unary plus (identity operation)
   */
  def unary_+ = this

  /**
   * Subtract polynomials.
   */
  def - (that: Polynomial[T]): Polynomial[T] = Polynomial.apply(
    for (degree <- (this.coefficients.keySet ++ that.coefficients.keySet)) 
      yield (degree, ev.minus(this.get(degree), that.get(degree)))
  )

  /**
   * Negate polynomial
   */
  def unary_- = Polynomial.zero[T] - this

  /**
   * Multiply polynomials.
   */
  def * (that: Polynomial[T]): Polynomial[T] = Polynomial.apply(
      for (degree <- 0 to this.degree + that.degree)
        yield (degree, coefForDegree(this, that, degree))
  )

  /**
   * Divide polynomials (aka, quotient).
   */
  def / (that: Polynomial[T]): Polynomial[T] = Polynomial.modulusQuotient(this, that)._2

  /**
   * Remainder after division (aka, modulus).
   */
  def % (that: Polynomial[T]): Polynomial[T] = Polynomial.modulusQuotient(this, that)._1

  /**
   * Compute the coefficient for the given degree.
   */
  private def coefForDegree(x: Polynomial[T], y: Polynomial[T], degree: Int): T = {
    val productsOfDegree = for (i <- 0 to degree) yield ev.times(x.get(i), y.get(degree - i))
    productsOfDegree.foldLeft(ev.zero)(ev.plus(_, _)) // return the sum
    }

  /**
   * Pretty string formatting for a polynomial
   */
  override def toString: String = f"${this.getClass.getName}(" 
    + coefficients.toSeq.sortBy(_._1)
      .map(formatMonomial)
      .reduceOption(_ + " + " + _)
      .getOrElse("0")
    + ")"

  private def formatMonomial(degree: Int, scalar: T): String = 
    if (degree == 0 && scalar == ev.one) { 
      "1" 
    } else {
      formatScalar(scalar) + formatExponent(degree)
    }

  private def formatScalar(scalar: T): String = 
    if (scalar == ev.one) { "" } 
    else { f"${scalar}" }

  private def formatExponent(degree: Int): String = degree match {
    case 0 => ""
    case 1 => "X"
    case _ => f"X^${degree}"
  }

  implicit val polynomialEuclideanRing: EuclideanRing[Polynomial[T]] = 
    new EuclideanRing[Polynomial[T]] {
      def negate(x: Polynomial[T]) = -x
      def plus(x: Polynomial[T], y: Polynomial[T]) = x + y
      override def minus(x: Polynomial[T], y: Polynomial[T]) = x - y
      def times(x: Polynomial[T], y: Polynomial[T]) = x * y

      def modulus(x: Polynomial[T], y: Polynomial[T]) = x % y
      def quotient(x: Polynomial[T], y: Polynomial[T]) = x / y
      override def modulusQuotient(x: Polynomial[T], y: Polynomial[T]) = Polynomial.modulusQuotient[T](x, y)

      def zero = Polynomial.zero[T]
      def one = Polynomial.one[T]
    }

  /**
   * Evaluate the polynomial at a point
   */
  def apply(x: T): T = coefficients.toSeq.sortBy(_._1)
    .foldRight((degree + 1, ev.zero))({
      case ((m, coeff), (n, acc)) => {
        (m, ev.plus(ev.times(ev.pow(x, n - m), acc), coeff)) // (m, acc*x^(n-m) + coeff)
      }
    })._2
}

object Polynomial {

  def apply[T](coef: (Int, T)*)(implicit ev: Field[T]): Polynomial[T] 
    = Polynomial(reduceCoeff(Map.from(coef)))

  def apply[T](coef: IterableOnce[(Int, T)])(implicit ev: Field[T]): Polynomial[T] 
    = Polynomial(reduceCoeff(Map.from(coef)))

  def empty[T](implicit ev: Field[T]) = Polynomial(reduceCoeff(Map.empty[Int, T]))

  def zero[T](implicit ev: Field[T]) = empty[T]

  def one[T](implicit ev: Field[T]) = Polynomial(reduceCoeff(Map((0, ev.one))))

  def modulus[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = modulusQuotient(x, y)._1
  def quotient[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = modulusQuotient(x, y)._2
  def modulusQuotient[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): (Polynomial[T], Polynomial[T]) = 
    (x.degree, y.degree) match {
      case (x_deg, y_deg) if x_deg < y_deg => (x, zero[T])
      case (x_deg, y_deg) if x_deg == y_deg => {
        val scalar: T = ev.div(x.get(x_deg), y.get(y_deg))
        (x - scalar * y, scalar)
      }
      case (x_deg, y_deg) => {
        // TODO: Handle divide by zero?
        val scalar: T = ev.div(x.get(x_deg), y.get(y_deg))
        val scalePoly = Polynomial.apply((x_deg - y_deg, scalar))
        val subSolution = modulusQuotient(x - scalePoly * y, y)
        (subSolution._1, scalePoly + subSolution._2)
    }
  }
  /**
   * Compute the GCD of polynomials
   */
  @tailrec
  def gcd[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] =
    if (x.degree < y.degree) {
      gcd[T](y, x)(ev)
    } else if (y == Polynomial.zero[T](ev)) {
      x
    } else {
      gcd[T](y, x % y)(ev)
    }
  
  /**
   * Compute the LCM of polynomials
   */
  def lcm[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = 
    if (x == Polynomial.zero[T](ev)) { Polynomial.zero[T](ev) }
    else { (x/gcd[T](x, y)(ev)) * y }

  // Note: this silently drops negative orders.
  private def reduceCoeff[T](coeff: Map[Int, T])(implicit ev: Field[T]) = coeff.filter((i, v) => v != ev.zero && i >= 0)
}

/**
 * Implicitly convert a scalar to a polynomial when needed.
 */
implicit def scalarConversion[T](scalar: T)(implicit ev: Field[T]): Polynomial[T] = Polynomial.apply[T]((0, scalar))
