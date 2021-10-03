import Prime._
import algebra.ring.Field

// TODO: Polynomials over fields
/** 
  How do we want this to look? A polynomial should be something like this:
     pf = PrimeField(Prime.next(5))
     poly = Polynomial(pf(1), pf(2), pf(3)) // Maybe construct with an array instead?
     println(poly) // "1 + 2 X + 3 X^2"
     poly(pf(2)) // "Element(2)"

     // Implement addition / subtraction / multiplication / division
 * 
*/

class Polynomial[T <: Field[T]](coefficients: Array[T]) {

}


object Polynomial {
  def main() = {

  }
}
