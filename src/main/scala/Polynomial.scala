import Prime._
import algebra.ring.Field

/**
 * Polynomial in one variable over a field.
 */
case class Polynomial[T] private (private val coefficients: Map[Int, T])(implicit ev: Field[T]) {

  /**
   * Degree of the polynomial.
   */
  val degree = (coefficients.keySet + 0).max

  /**
   * Get the coefficient for the exponent.
   */
  def get(degree: Int) = coefficients.getOrElse(degree, ev.zero)

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
   * Divide polynomials.
   */
  def / (that: Polynomial[T]): Polynomial[T] = Polynomial.modDiv(this, that)._1

  /**
   * Remainder after division
   */
  def % (that: Polynomial[T]): Polynomial[T] = Polynomial.modDiv(this, that)._2

  /**
   * Compute the coefficient for the given degree.
   */
  private def coefForDegree(x: Polynomial[T], y: Polynomial[T], degree: Int): T = {
    val productsOfDegree = for (i <- 0 to degree) yield ev.times(x.get(i), y.get(degree - i))
    productsOfDegree.foldLeft(ev.zero)(ev.plus(_, _)) // return the sum
    }

  override def toString: String = f"${this.getClass.getName}(" 
    + coefficients.toSeq.sortBy(_._1).map((i, v) => f"${v} X^${i}").reduce(_ + " + " + _) 
    + ")"
  
  implicit val polynomialField: Field[Polynomial[T]] = new Field[Polynomial[T]] {
    def negate(x: Polynomial[T]) = -x
    def plus(x: Polynomial[T], y: Polynomial[T]) = x + y
    override def minus(x: Polynomial[T], y: Polynomial[T]) = x - y
    def times(x: Polynomial[T], y: Polynomial[T]) = x * y
    def div(x: Polynomial[T], y: Polynomial[T]) = x / y
    def zero = Polynomial.zero[T]
    def one = Polynomial.one[T]
  }
}

object Polynomial {

  def apply[T](coef: (Int, T)*)(implicit ev: Field[T]): Polynomial[T] 
    = Polynomial(reduceCoeff(Map.from(coef)))

  def apply[T](coef: IterableOnce[(Int, T)])(implicit ev: Field[T]): Polynomial[T] 
    = Polynomial(reduceCoeff(Map.from(coef)))

  def empty[T](implicit ev: Field[T]) = Polynomial(reduceCoeff(Map.empty[Int, T]))

  def zero[T](implicit ev: Field[T]) = empty[T]

  def one[T](implicit ev: Field[T]) = Polynomial(reduceCoeff(Map((0, ev.one))))

  def gcd[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = ???

  def quotient[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = x / y
  def remainder[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): Polynomial[T] = x % y

  def modDiv[T](x: Polynomial[T], y: Polynomial[T])(implicit ev: Field[T]): (Polynomial[T], Polynomial[T]) = 
    if (x == zero[T]){
      (x, y)
    }
    else {
      (x, y)
    }

  private def reduceCoeff[T](coeff: Map[Int, T])(implicit ev: Field[T]) = coeff.filter((i, v) => v != ev.zero)
}
