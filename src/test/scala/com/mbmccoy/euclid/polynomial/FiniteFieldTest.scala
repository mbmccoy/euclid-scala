package com.mbmccoy.euclid.polynomial

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._
import scala.language.implicitConversions
import scala.math.pow
import algebra.ring.Field


class FiniteFieldTest extends AnyFlatSpec with should.Matchers {
    val two_p = Prime.next(2)
    val three_p = Prime.next(3)
    val five_p = Prime.next(5)

    // Use in tests with a single CF
    val cf = FiniteField(three_p, exponent=2)
    val zero = cf.zero
    val one = cf.one
    val x = cf(Polynomial((1, cf.primeField(1))))
    val two = cf(Polynomial((0, cf.primeField(2))))

    // Use in tests with multiple CFs
    val cf1 = FiniteField(five_p, exponent=3)
    val cf2 = FiniteField(two_p, exponent=12)

    "ConwayField" should "have the correct prime field" in {
        cf1.primeField.p should be(five_p)
        cf2.primeField.p should be(two_p)
    }

    "Characteristic" should "be computed correctly" in {
        cf1.characteristic should be(5)        
        cf2.characteristic should be(2)
    }

    "Order" should "be computed correctly" in {
        cf1.order should be(pow(5, 3))        
        cf2.order should be(pow(2, 12))
    }

    "Elements" should "be able to be added" in {
        zero + one should be (one)
        one + one should be (two)
        two + one should be (zero)
        two + x should be (cf(Polynomial((0, cf.primeField(2)), (1, cf.primeField(1)))))
    }

    "Elements" should "be able to be subtracted" in {
        one - zero should be (one)
        one - one should be (zero)
        two - one should be (one)
        two - x should be (cf(Polynomial((0, cf.primeField(2)), (1, cf.primeField(-1)))))
        two + (-x) should be (two - x)
    }

    "Elements" should "be able to be multiplied" in {
        one * zero should be (zero)
        one * one should be (one)
        two * one should be (two)
        one * x should be (x)
        (+two) * (one + x) should be (cf(Polynomial((0, cf.primeField(2)), (1, cf.primeField(2)))))
    }

    "Elements" should "be able to be divided" in {
        zero / one should be (zero)
        zero / two should be (zero)
        two / one should be (two)
        (two * x) / two should be (x)
        two * (one - two * x + x * x) / (x - one) should be (two * (x - one))
    }

    "Fields of characteristic one" should "be available" in {
        val p = Prime.next(65537) 
        val exponent = 1
        val ff = FiniteField(p, exponent)
        val pf = PrimeField(p)
        ff.primeField.order should be (pf.order)
        ff.conwayPolynomial.toString should be ("1 + X")
    }

    "Random" should "sample elements" in {
        val p = Prime.next(3)
        val exponent = 4
        val ff = FiniteField(p, exponent)
        for (i <- 0 until 100) {
            val sample = ff.random
            val product = sample * (p.toInt + 1)
            product should be (sample)
        }
    }

    "Product with ints" should "be sane" in {
        val el = (cf1.one + cf1.one)
        (el * 0) should be (cf1.zero)
        (el * 1) should be (el)
        (el * cf1.primeField.order.toInt) should be (cf1.zero)
    }
}
