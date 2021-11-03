package com.mbmccoy.euclid.polynomial

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._
import scala.language.implicitConversions

import algebra.ring.Field
import FieldOps._


class FieldOpsTest extends AnyFlatSpec with should.Matchers {
    val pf: PrimeField = PrimeField(Prime.next(7))

    "batchInversion" should "work with one element" in {
        batchInverse(pf(5) :: List.empty) should be (pf(3) :: List.empty)
    }

    "batchInversion" should "work with no elements" in {
        batchInverse(List.empty[pf.Element]) should be (List.empty[pf.Element])
    }

    "batchInversion" should "work in the happy case" in {
        //val value = pf(2) #:: pf(3) #:: pf(5) #:: pf(4) #:: LazyList.empty
        val elements: List[pf.Element] = pf(2) :: pf(4) :: pf(5) :: pf(3) :: pf(6) :: List.empty
        val inverse = batchInverse(elements)
        // Check that elements times inverse is one
        inverse.zip(elements).map(_*_).map(_ should be (pf.one))
    }

    "lagrangeInterpolation" should "work in the happy case" in {
        val elements = pf(0) :: pf(1) :: pf(2) :: pf(4) :: pf(3) :: pf(5) :: pf(6) :: List.empty
        val values = pf(2) :: pf(2) :: pf(2) :: pf(2) :: pf(2) :: pf(2) :: pf(2) :: List.empty
        val result = lagrangeInterpolation(elements.zip(values))
        result.toString should be ("2")
    }

    "rootPolynomials" should "work" in {
        val elements = pf(0) :: pf(1) :: pf(2) :: pf(4) :: pf(3) :: pf(5) :: pf(6) :: List.empty
        val rootPoly = rootPolynomial(elements)
        rootPoly.toString should be ("6X + X^7")  // same as X^7 - 1
        elements map {el =>
            rootPoly(el) should be (pf.zero)
        }
    }
}
