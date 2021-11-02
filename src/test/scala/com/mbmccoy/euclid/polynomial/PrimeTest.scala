package com.mbmccoy.euclid.polynomial

import org.junit.Test
import org.junit.Assert._

class PrimeTest {
    @Test
    def testNext = {
        assertEquals(17, Prime.next(14).toInt)
        assertEquals(13, Prime.next(13).toInt)
    }

    @Test
    def testFrom = {
        val isNotPrime = BigInt("283483284848423838348223483284833384883848381162164232344")
        val isPrime = BigInt("283483284848423838348223483284833384883848381162164232369")
        assertEquals(None, Prime.from(isNotPrime))
        assertEquals(Some(isPrime), Prime.from(isPrime).map(p => p.toBigInt))
    }
}