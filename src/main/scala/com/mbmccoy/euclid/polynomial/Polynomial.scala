package com.mbmccoy.euclid.polynomial

import algebra.ring.Field
import scala.language.implicitConversions
import scala.annotation.tailrec

/**
 * Polynomial in one variable over a field.
 */
case class Polynomial[T] private (
    private val coefficients: Map[Int, T], 
    val formalVariable: String = "X"
  )(implicit ev: Field[T]) {

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
      // TODO: Use Fourier transform over finite fields
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
  override def toString: String = coefficients.toSeq.sortBy(_._1)
      .map(formatMonomial)
      .reduceOption(_ + " + " + _)
      .getOrElse("0")

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
    case 1 => f"${formalVariable}"
    case _ => f"${formalVariable}^${degree}"
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
   * Sorted coefficients of the polynomial, from highest to lowest degree.
   * 
   * We require the (0, 0) coefficient for the algorithm used for
   * point evaluation below.
   */
  private lazy val sortedCoefs: List[(Int, T)] = {
    val coef = get(0) match {
      case 0 => coefficients.toList
      case _ => (0, ev.zero) :: coefficients.toList
    }
    coef.sortBy(-_._1)
  }

  /**
   * Evaluate the polynomial at a point
   */
  def apply(x: T): T = sortedCoefs
    .foldLeft((degree + 1, ev.zero))({
      case ((n, acc), (m, coeff)) => {
        (m, ev.plus(ev.times(ev.pow(x, n - m), acc), coeff)) // (m, acc*x^(n-m) + coeff)
      }
    })._2

    /**
     * Create a new polynomial with a different formal variable.
     */
    def withFormalVariable(newFormalVariable: String) = new Polynomial(coefficients, newFormalVariable)
}

object Polynomial {

  def apply[T](coef: (Int, T)*)(implicit ev: Field[T]): Polynomial[T] 
    = new Polynomial(reduceCoeff(Map.from(coef)))

  def apply[T](coef: IterableOnce[(Int, T)])(implicit ev: Field[T]): Polynomial[T] 
    = new Polynomial(reduceCoeff(Map.from(coef)))

  def apply[T](coef: IterableOnce[(Int, T)], formalVariable: String)(implicit ev: Field[T]): Polynomial[T] 
    = new Polynomial(reduceCoeff(Map.from(coef)), formalVariable=formalVariable)

  def empty[T](implicit ev: Field[T]) = Polynomial(reduceCoeff(Map.empty[Int, T]))

  def zero[T](implicit ev: Field[T]) = empty[T]

  def one[T](implicit ev: Field[T]) = Polynomial(reduceCoeff(Map((0, ev.one))))

  def modulus[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = modulusQuotient(x, y)._1
  def quotient[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = modulusQuotient(x, y)._2

  @tailrec
  private def modulusQuotientHelper[T](x: Polynomial[T], y: Polynomial[T], prevScale: Polynomial[T])(implicit ev: Field[T])
    : (Polynomial[T], Polynomial[T], Polynomial[T]) = {

      (x.degree, y.degree) match {
      case (x_deg, y_deg) if x_deg < y_deg => (x, zero[T], prevScale)
      case (x_deg, y_deg) if x_deg == y_deg => {
        val scalar: T = ev.div(x.get(x_deg), y.get(y_deg))
        (x - scalar * y, scalar, prevScale)
      }
      case (x_deg, y_deg) => {
        // TODO: Handle divide by zero?
        val scalar: T = ev.div(x.get(x_deg), y.get(y_deg))
        val scalePoly = Polynomial.apply((x_deg - y_deg, scalar))
        modulusQuotientHelper(x - scalePoly * y, y, scalePoly + prevScale)
      }
    }
  }

  def modulusQuotient[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): (Polynomial[T], Polynomial[T]) = {
    val result = modulusQuotientHelper(x, y, zero[T])
    (result._1, result._2 + result._3)
  }
    /*
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
        (subSolution._1, subSolution._2 + scalePoly)
    } 
  } */

  /**
   * Compute the GCD of polynomials.
   */
  @tailrec
  def gcd[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] =
    if (x.degree < y.degree) {
      gcd[T](y, x)(ev)
    } else if (y == Polynomial.zero[T](ev)) {
      x / x.get(x.degree)
    } else {
      gcd[T](y, x % y)(ev)
    }

  /**
    * Bézout polynomials via the extended Euclidean algorithm.
    *
    * @return Tuple (r, s, t) such that r == a*s + b*t and r == gcd(a, b)
    */
  def bezout[T](a: Polynomial[T], b: Polynomial[T])(implicit ev: Field[T]): (Polynomial[T], Polynomial[T], Polynomial[T]) = {
    val zero = Polynomial.zero[T](ev)
    val one = Polynomial.one[T](ev)
    @tailrec
    def bezoutR(r0: Polynomial[T]=a, s0: Polynomial[T]=one, t0: Polynomial[T]=zero,
                r1: Polynomial[T]=b, s1: Polynomial[T]=zero, t1: Polynomial[T]=one):
    (Polynomial[T], Polynomial[T], Polynomial[T]) = 
      if (r1 == zero) {
        val leadingCoeff = r0.get(r0.degree)
        // Rescale to a monic polynomial
        (r0 / leadingCoeff, s0 / leadingCoeff, t0 / leadingCoeff)
      } else {
        val q = r0 / r1
        val m = r0 % r1
        bezoutR(r1, s1, t1, m, s0 - q * s1, t0 - q * t1)
      }
    bezoutR()
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
