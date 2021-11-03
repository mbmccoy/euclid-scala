package com.mbmccoy.euclid.polynomial

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.io.Source
import org.json4s._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.json4s.jackson.JsonMethods._
import scala.util.Using

import algebra.ring.Field


/**
 * Finite fields of order p^e represented by Conway Polynomials.
 * 
 * Note that while all finite fields have Conway polynomials, these 
 * polynomials are expensive to compute, and are known only in special
 * cases [1]. An error will occur if you attempt to construct a
 * field for which we do not have a polynomial.
 * 
 * [1] http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html
 */
class FiniteField(p: Prime, exponent: Int, conwayExponents: Iterable[(Int, Int)]=Nil) {
    /**
     * Base field of order p^e.
     * 
     * The full field is represented as polynomials over the base field.
     */
    val primeField: PrimeField = PrimeField(p)

    /**
     * The characteristic of this field.
     */
    val characteristic: BigInt = primeField.p.toBigInt

    /**
     * The order (size) of the prime field.
     */
    val order: BigInt = primeField.p.toBigInt.pow(exponent)

    /**
     * The Conway polynomial for this field.
     * 
     * Note that this polynomial is not an element of the prime field.
     * 
     * Elements of the field are constructed modulo this polynomial.
     */
    val conwayPolynomial: Polynomial[primeField.Element] = {
        val _exponents: IterableOnce[(Int, Int)] = {
            if (conwayExponents.isEmpty) {
            exponent match {
                case 1 => (0, 1) :: (1, 1) :: Nil
                case _ =>                 
                    ConwayData.get(p, exponent).exponents.zipWithIndex.map(x => (x._2, x._1))
            }}
            else
                conwayExponents
        }
        Polynomial(_exponents.iterator.map(x=>(x._1, primeField(x._2))))
    }

    /** 
     * Element of this Conway field.
     */
    case class Element private (private val value: Polynomial[primeField.Element]) {
        // Note: We choose not to extend the polynomial definition because the
        // elements of a field are not polynomials per se; they are just represented
        // by polynomials internally.
        def + (that: Element) = Element.apply(this.value + that.value)
        def unary_+ = this

        def - (that: Element) = Element.apply(this.value - that.value)
        def unary_- = Element.zero - this

        def * (that: Element): Element = Element.apply(this.value * that.value)
        def * (that: Int): Element = fieldImpl.sumN(this, that)

        def / (that: Element): Element = this * that.reciprocal

        def pow(that: Int): Element = fieldImpl.pow(this, that)

        def reciprocal: Element = {
            // The Conway polynomial is irreducible, so r==1. Thus,
            // 1 == s * this + t * conwayPolynomial === s * this (mod conwayPolynomial)
            // Thus, s == 1/this.
            val (r, s, t) = Polynomial.bezout[primeField.Element](this.value, conwayPolynomial)
            Element.apply(s)
        }
        def toPolynomial: Polynomial[primeField.Element] = value
        override def toString: String = f"${this.toPolynomial}"
    }

    object Element {
        def apply(value: Polynomial[primeField.Element]): Element = new Element(normalize(value))        
        val zero: Element = new Element(Polynomial((0, primeField.zero)))
        val one: Element = new Element(Polynomial((0, primeField.one)))
        private val order: BigInt = p.toBigInt    
        private def normalize(value: Polynomial[primeField.Element]): Polynomial[primeField.Element] = 
            value % conwayPolynomial match {
                // TODO: Does this ensure that the highest-order term is positive?
                case v if v.get(v.degree).toInt < 0 => (v + conwayPolynomial) % conwayPolynomial
                case v => v
            }
    }

    implicit val fieldImpl: Field[Element] = new Field[Element] {
        lazy val zero = Element.zero
        lazy val one = Element.one

        def negate(x: Element): Element = (one - x)
        def plus(x: Element, y: Element): Element = (x + y)
        def combine(x: Element, y: Element): Element = (x + y)
        def product(x: Element, y: Element): Element = x * y
        def times(x: Element, y: Element): Element = x * y
        def div(x: Element, y: Element): Element = x / y
        override def reciprocal(x: Element) = x.reciprocal
    }

    /**
     * Sample a random element from this field.
     */
    def random: Element = {
        val coefficients = for (i <- 0 until exponent) yield (i, primeField.random)
        this.apply(Polynomial[primeField.Element](
            coefficients
        ))
    }

    def zero = this.Element.zero
    def one = this.Element.one

    def apply(polynomial: Polynomial[primeField.Element]): Element = Element.apply(polynomial)
    def apply(coef: Iterable[(Int, Int)]): Element = Element.apply(
        Polynomial[primeField.Element].apply(
            coef.map(v => (v._1, primeField(v._2)))
        )
    )

    @targetName("applyCoefficients")  // Handle type erasure
    def apply(coefficients: Iterable[Int]): Element = 
        apply(coefficients.zipWithIndex.map(x => (x._2, x._1)))
}


private case class ConwayData(p: Prime, order: Int, exponents: List[Int])

private object ConwayData{
    private lazy val conwayData: Map[(Prime, Int), ConwayData] = {
        println("Loading Conway polynomial database...")
        implicit val formats: Formats = DefaultFormats
        val fileStream = getClass.getResourceAsStream("/conwayPolynomials.json")
        val conwayPolynomials = parse(fileStream)
        fileStream.close
        val result = {
            (conwayPolynomials \ "allPolynomials") match {
                case x: JArray =>
                    Some(x)
                case _ => 
                    None
        }}.map(
            _.values.flatMap(v => v match {
                case vList: List[Any] => {
                    Some((vList(0), vList(1), vList(2)) )
                }
                case _ => None
            }).flatMap(_ match {
                case (p, n, a): (BigInt, BigInt, List[BigInt]) => 
                    Some((Prime.from(p), n.toInt, a.map(_.toInt)))
                case _ => None
            }).flatMap(_ match {
                case (p, n, a): (Some[Prime], Int, List[Int]) =>
                    Some(((p.get, n), ConwayData(p.get, n, a)))
                case _ => 
                    None
            }).toMap
        ).get
        println("Done.")
        result
    }
    def get(p: Prime, order: Int): ConwayData = conwayData.get((p, order)).get
    def get(p: Int, order: Int): ConwayData = Prime.from(p).flatMap(conwayData.get(_, order)).get
}
