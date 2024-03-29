
import cats.Group
import algebra.ring.Field

import Euclid._

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

        override def toString: String = f"${this.toInt}"
    }

    object Element {
        def apply(value: BigInt): Element = new Element(normalize(value))

        def zero = apply(0)
        def one = apply(1)

        private val order: BigInt = p.toBigInt    
        private def normalize(value: BigInt): BigInt = value % order match {
            case v if v < 0 => v + order
            case v => v
        }
    }

    implicit val elementField: Field[Element] = new Field[Element] {
        lazy val zero: Element = pf.Element.zero
        lazy val one: Element = pf.Element.one

        def negate(x: Element): Element = zero - x
        def plus(x: Element, y: Element): Element = x + y
        def combine(x: Element, y: Element): Element = x + y
        def product(x: Element, y: Element): Element = x * y
        def times(x: Element, y: Element): Element = x * y
        def div(x: Element, y: Element): Element = x / y
    }

    def apply(value: BigInt): Element = Element.apply(value)
    def apply(value: Int): Element = Element.apply(BigInt.apply(value))

    val zero = this.Element.zero
    val one = this.Element.one

    override def toString() = s"${this.getClass.getName}(${p.toBigInt.toString})"
}

