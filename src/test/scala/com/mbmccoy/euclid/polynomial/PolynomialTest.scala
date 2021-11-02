package com.mbmccoy.euclid.polynomial

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._

class PolynomialTest extends AnyFlatSpec with should.Matchers {
    val pf = PrimeField(Prime.next(5))

    "Polynomial" should "have a nice string representation" in {
        Polynomial.zero[pf.Element].toString() should be ("0")
        Polynomial.one[pf.Element].toString() should be ("1")
        val complexPoly = Polynomial((0, pf(4)), (1, pf(3)), (2, pf(1)), (3, pf(4)), (4, pf(0)), (5, pf(3)))
        complexPoly.toString() should be ("4 + 3X + X^2 + 4X^3 + 3X^5")
    }

    "withFormalVariable" should "give a new formal variable" in {
        val complexPoly = Polynomial((0, pf(4)), (1, pf(3)), (2, pf(1)), (3, pf(4)), (4, pf(0)), (5, pf(3)))
        complexPoly.withFormalVariable("Y").toString() should be ("4 + 3Y + Y^2 + 4Y^3 + 3Y^5")
    }

    "Two polynomials" should "be able to to be compared" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val cPoly = Polynomial((0, pf(2)), (1, pf(4)))

        aPoly should be (aPoly)
        aPoly should be (cPoly)
        aPoly should not be (bPoly)
    }

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
        +aPoly should be (aPoly)
    }

    "Scalar add" should "work on both sides" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val scalar = pf(2)
        aPoly + scalar should be (Polynomial((0, pf(4)), (1, pf(4))))
        scalar + aPoly should be (Polynomial((0, pf(4)), (1, pf(4))))
    }

    "Two polynomials" should "be able to be subtracted" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val sumPoly = aPoly - bPoly
        sumPoly should be (Polynomial((1, pf(4)), (2, pf(-3))))
    }

    "Scalar subtract" should "work on both sides" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        val scalar = pf(1)
        aPoly - scalar should be (Polynomial((0, pf(1)), (1, pf(4))))
        scalar - aPoly should be (Polynomial((0, pf(-1)), (1, pf(-4))))
    }

    "Unary subtract" should "negate the polynomial" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)))
        -aPoly should be (Polynomial((0, pf(-2)), (1, pf(-4))))
    }

    "Two polynomials" should "be able to be multiplied" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)), (3, pf(1)))
        val bPoly = Polynomial((0, pf(2)), (2, pf(3)))
        val mulPoly = aPoly * bPoly

        mulPoly should be (
            Polynomial((0, pf(4)), (1, pf(3)), (2, pf(1)), (3, pf(4)), (4, pf(0)), (5, pf(3)))
        )
    }

    "Scalar multiply" should "work on both sides" in {
        val aPoly = Polynomial((0, pf(2)), (1, pf(4)), (3, pf(1)))
        val scalar = pf(3)
        val rMult = aPoly * scalar
        val lMult = scalar * aPoly
        rMult should be (Polynomial((0, pf(1)), (1, pf(2)), (3, pf(3))))
        lMult should be (rMult)
    }

    "Two polynomials" should "be have modulus and quotient" in {
        val aPoly = Polynomial((3, pf(4)), (2, pf(3)), (0, pf(2)))  // 5 x^3 + 3 x^2 + 2
        val bPoly = Polynomial((1, pf(1)), (0, pf(1))) // x + 1
        val (m, q) = Polynomial.modulusQuotient(aPoly, bPoly)
        m should be (Polynomial((0, pf(1))))
        q should be (Polynomial((2, pf(4)), (1, pf(-1)), (0, pf(1))))
        q * bPoly + m should be (aPoly)

        val (m2, q2) = Polynomial.modulusQuotient(bPoly, aPoly)
        q2 should be (Polynomial.zero[pf.Element])
        m2 should be (bPoly)

        aPoly / bPoly should be (q)
        aPoly % bPoly should be (m)
    }

    "Modulus with the unit polynomial" should "be zero" in {
        val aPoly = Polynomial((3, pf(4)), (2, pf(3)), (0, pf(2))) 
        aPoly % Polynomial.one[pf.Element] should be (Polynomial.zero[pf.Element])
    }

    "A polynomial" should "be able to be evaluated" in {
        val aPoly = Polynomial((3, pf(4)), (2, pf(3)), (0, pf(2)))
        aPoly(pf.zero) should be (pf(2))
        aPoly(pf.one) should be (pf(4))
        aPoly(pf(2)) should be (pf(1))
    }

    "Polynomials" should "have gcds and lcms" in {
        val aPoly = Polynomial((4, pf(4)), (2, pf(3)), (0, pf(2)))  // 5 x^4 + 3 x^2 + 2
        val bPoly = Polynomial((1, pf(1)), (0, pf(1))) // x + 1
        val cPoly = Polynomial((2, pf(2)), (1, pf(3)), (0, pf(1))) // 2x^2 + 3x + 1
        val g = Polynomial.gcd(aPoly*bPoly, bPoly*cPoly)
        val l = Polynomial.lcm(aPoly*bPoly, bPoly*cPoly)
        g should be (bPoly)
        l should be (aPoly * bPoly * cPoly)

        // Check to ensure it's a monic polynomial
        val g2 = Polynomial.gcd(pf(2)*aPoly*bPoly, pf(2)*cPoly*bPoly)
        g2 should be (bPoly)
    }

    "Bezout's algorithm" should "work with polynomials" in {
        val aPoly = Polynomial((4, pf(4)), (2, pf(3)), (0, pf(2)))  // 5 x^4 + 3 x^2 + 2
        val bPoly = Polynomial((1, pf(1)), (0, pf(1))) // x + 1
        val cPoly = Polynomial((2, pf(2)), (1, pf(3)), (0, pf(1))) // 2x^2 + 3x + 1
        val one = Polynomial.one[pf.Element]
        val g = Polynomial.gcd(aPoly*bPoly, bPoly*cPoly)

        val (r, s, t) = Polynomial.bezout[pf.Element](aPoly*bPoly, bPoly*cPoly)
        val (r2, s2, t2) = Polynomial.bezout[pf.Element](pf(2)*aPoly*bPoly, pf(2)*bPoly*cPoly)

        g should be (bPoly)
        r should be (g)
        r2 should be (g)

        s * aPoly + t * cPoly should be (one)
        s2 * pf(2) * aPoly + t2 * pf(2) * cPoly should be (one)
    }
}
