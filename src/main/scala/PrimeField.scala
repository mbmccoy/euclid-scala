
import cats.Group
import algebra.ring.Field
import algebra.ring.AdditiveCommutativeGroup
import scala.{specialized => sp}

import Euclid._
import Prime._
import javax.lang.model.util.Elements


/** 
 * Prime field for a given prime. 
 */
class PrimeField(val p: Prime) { pf =>

    /** 
     * Element of a prime field.
     */
    case class Element private (val value: BigInt) {

        def + (that: Element) = apply(this.value + that.value)
        def - (that: Element) = apply(this.value - that.value)
        def * (that: Element): Element = apply(this.value * that.value)
        def / (that: Element): Element = modDiv(this.value, Element.order, that.value).map(apply).get

        def toInt = value.toInt
        def toBigInt = value
    }

    object Element extends Field[Element] {
        lazy val zero: Element = pf.zero
        lazy val one: Element = pf.one
        def negate(x: Element): Element = zero - x
        def plus(x: Element, y: Element): Element = x + y

        def combine(x: Element, y: Element): Element = x + y
        def inverse(x: Element): Element = zero - x
        def product(x: Element, y: Element): Element = x * y
        def productInverse(x: Element): Option[Element] = modDiv(1, Element.order, x.value).map(apply)

        def times(x: Element, y: Element): Element = x * y
        def div(x: Element, y: Element): Element = x / y
        def apply(value: BigInt): Element = new Element(normalize(value))
        private val order: BigInt = p.toBigInt    
        private def normalize(value: BigInt): BigInt = value % order match {
            case v if v < 0 => v + order
            case v => v
        }
    }

    def apply(value: BigInt): Element = Element.apply(value)

    val zero = Element.apply(0)
    val one = Element.apply(1)

    override def toString() = s"${this.getClass.getName}(${p.toBigInt.toString})"
}


object PrimeField {

    // For testing
    def main(args: Array[String]): Unit = {

    }
}
