package com.yevhenii.parsing.validation

import org.scalatest.{Matchers, WordSpec}

class ValidatorSpec extends WordSpec with Matchers {

  "Validator.validate" should {
    "validate correct expressions" in {
      val expressions = List(
        "1. + 2.0 * sin(pi / 2)",
        "2+2*2",
        "1+2*(3+4*5)",
        "8/2/2",
        "8-2-2"
      )

      expressions
        .map(x => Validator.validate(Set("pi", "sin"))(x))
        .foreach(res => res.isRight shouldBe true)
    }

    "validate incorrect expressions" in {
      val expressions = List(
        ".1. + 2.0 * sin(pi / 2)",
        "2+2*2+",
        "1+2*((3+4*5)",
        "8/2/2)",
        "8-(2-2-)",
        "8-(+2-2)",
        "8-cos(2)"
      )

      expressions
        .map(x => Validator.validate(Set("pi", "sin"))(x))
        .map { x => println(x); x }
        .foreach(res => res.isRight shouldBe false)
    }
  }
}
