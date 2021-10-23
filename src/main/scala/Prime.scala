import scala.annotation.tailrec

/** Prime numbers. */
case class Prime private (private val value: BigInt) {
    def toBigInt = value
    def toInt = value.toInt
}

object Prime {

    /** Return Some(prime) if value is a prime, otherwise return None. */
    def from(value: BigInt, certainty: Int = 64): Option[Prime] = 
        (value.isProbablePrime(certainty), value) match {
            case (true, v) => Some(Prime(v))
            case  _ => None
        }

    /** Return the next prime, possibly including this value. 
     * 
     * Thus, Prime.next(3).value == 3, and Prime.next(4).value == 5
     */
    @tailrec
    def next(value: BigInt, certainty: Int = 64): Prime = (value.isProbablePrime(certainty), value) match {
        case (true, v) => Prime(v)
        case (_, v) => next(value+1)
    }
}
