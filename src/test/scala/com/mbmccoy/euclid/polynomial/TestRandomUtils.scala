package com.mbmccoy.euclid.polynomial

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import flatspec._
import matchers._

class TestRandomUtils extends AnyFlatSpec with should.Matchers {
  "Sampling" should "be in range" in {
      for (i <- 0 until 100) {
        val x = RandomUtils.sample(100)
        x.toInt should be < 100
        x.toInt should be >= 0
      }
  }
}
