
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

        private val order: BigInt = p.toBigInt    
        private def normalize(value: BigInt): BigInt = value % order match {
            case v if v < 0 => v + order
            case v => v
        }
    }

    implicit val elementField: Field[Element] = new Field[Element] {
        lazy val zero: Element = pf.zero
        lazy val one: Element = pf.one

        def negate(x: Element): Element = zero - x
        def plus(x: Element, y: Element): Element = x + y
        def combine(x: Element, y: Element): Element = x + y
        //def inverse(x: Element): Element = Euclid.modDiv(1, p.toBigInt, x.value).map(apply).get
        def product(x: Element, y: Element): Element = x * y
       // def productInverse(x: Element): Option[Element] = 
        def times(x: Element, y: Element): Element = x * y
        def div(x: Element, y: Element): Element = x / y
    }

    def apply(value: BigInt): Element = Element.apply(value)

    val zero = Element.apply(0)
    val one = Element.apply(1)

    override def toString() = s"${this.getClass.getName}(${p.toBigInt.toString})"
}

/**
 * Invert all field elements using Montgomery batch inversion
 */
def batchInverse[T](it: IterableOnce[T])(implicit ev: Field[T]): IterableOnce[T] = {
    val tList: List[T] = it.iterator.to(List) 
    lazy val products: LazyList[T] = tList.head #:: products.zip(tList.tail).map(t => ev.times(t._1, t._2))

    lazy val productsReversed = products.reverse
    lazy val tListReversed = tList.reverse
    lazy val inverses: LazyList[T] = ev.reciprocal(productsReversed.head) #:: inverses.zip(tListReversed).map(t => ev.times(t._1, t._2)).reverse
    inverses
}
