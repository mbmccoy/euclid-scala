package com.mbmccoy.euclid.polynomial

import scala.annotation.tailrec
import java.math.BigInteger
import java.security.SecureRandom


object RandomUtils {
  private val random = new SecureRandom()

  @tailrec
  def sample(max: BigInt): BigInt = {
    val result = new BigInt(new BigInteger(max.bitLength, random)) 
    if (result < max) result
    else sample(max)
  }

}
