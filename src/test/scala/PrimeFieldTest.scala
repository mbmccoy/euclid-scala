import org.junit.Test
import org.junit.Assert._
import scala.util.{Try, Failure, Success}


class PrimeFieldTest {
    
    @Test
    def testEquals = {
        val order = Prime.next(5)
        val pf = PrimeField(order)
        assertEquals(pf(2), pf(2))
        assertNotEquals(pf(2), pf(3))
        assertEquals(pf(2), pf(7))
        assertEquals(pf(2), pf(-8))
        assertEquals(pf.zero, pf(0))
        assertEquals(pf.one, pf(1))

        assertEquals(0, pf(0).toInt)
        assertEquals(BigInt(1), pf(1).toBigInt)
        
        // Check that 1 / 0 is a failure
        val result = Try { pf(1) / pf(0)}
        assertTrue(result.isFailure)
    }

    @Test
    def testOperations = {
        val order = Prime.next(5)
        val pf = PrimeField(order)
        val two = pf(2)
        val three = pf(3)

        assertEquals(pf(4), two * two)
        assertEquals(pf(4), three * three)
        assertEquals(pf(1), two * three)

        assertEquals(pf(0), two + three)
        assertEquals(pf(1), three + three)

        assertEquals(pf(1), two / two)
        assertEquals(pf(1), three / three)

        assertEquals(pf(4), two / three)
        assertEquals(pf(1), pf(4) * (two / three))
        assertEquals(pf(1) / pf(4), three / two)
    }

    @Test
    def testBatchInverse = {
        val order = Prime.next(124)
        val pf = PrimeField(order)
        val inverse = batchInverse(pf(2) #:: pf(3) #:: pf(13) #:: pf(123) #:: LazyList.empty)
        println(inverse.iterator.to(List))
    }
}