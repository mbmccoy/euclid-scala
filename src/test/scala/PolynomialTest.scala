import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._

class PolynomialTest extends AnyFlatSpec with should.Matchers {
    val pf = PrimeField(Prime.next(5))

    "An empty polynomial" should "have zero degree" in {
        val p = Polynomial.empty[pf.Element]
        p.degree should be (0)
    }

    "Zero coefficients" should "not appear in the polynomial" in {
        val p = Polynomial((0, pf.one), (1, pf.zero))
        p should be (Polynomial((0, pf.one)))
    }

    "A polynomial" should "know its degree" in {
        val coefficients = Map((0, pf(2)), (4, pf(4)))
        val p = Polynomial(coefficients)
        p.degree should be (4)
    }

    "Two polynomials" should "be able to be added" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val sumPoly = aPoly + bPoly
        sumPoly should be (Polynomial((0, pf(4)), (1, pf(4)), (2, pf(3))))
    }

    "Unary add" should "be an identity" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        aPoly should be (+aPoly)
    }

    "Two polynomials" should "be able to be subtracted" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val sumPoly = aPoly - bPoly

        sumPoly should be (Polynomial((1, pf(4)), (2, pf(-3))))

    }

    "Unary subtract" should "negate the polynomial" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        -aPoly should be (Polynomial((0, pf(-2)), (1, pf(-4))))
    }

    "Two polynomials" should "be able to to be compared" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val cPoly = Polynomial((0, pf(2)), (1, pf(4)))

        aPoly should be (aPoly)
        aPoly should be (cPoly)
        aPoly should not be (bPoly)

    }

    "Two polynomials" should "be able to be multiplied" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)), (3, pf(1)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val mulPoly = aPoly * bPoly

        mulPoly should be (
            Polynomial((0, pf(4)), (1, pf(3)), (2, pf(1)), (3, pf(4)), (4, pf(0)), (5, pf(3)))
        )
    }

}
