package com.mbmccoy.euclid.polynomial

import algebra.ring.{Ring, Field}
import scala.annotation.tailrec


trait EuclideanRing[T] extends Ring[T] {
    def modulus(x: T, y: T): T
    def quotient(x: T, y: T): T

    /**
     * Some implementations may want to override this for efficiency.
     */
    def modulusQuotient(x: T, y: T): (T, T) = (modulus(x, y), quotient(x, y))

    /**
     * Greatest common divisor
     */
    @tailrec
    final def gcd(x: T, y: T)(implicit ev: EuclideanRing[T]): T = 
        if (y == ev.zero) {
            x
        } else {
            ev.gcd(y, modulus(x, y))
        }

    /**
    * Least common multiple
    */
    def lcm(x: T, y: T)(implicit ev: EuclideanRing[T]): T 
        = ev.times(x, ev.quotient(y, gcd(x, y)))
}
