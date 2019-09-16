package com.yevhenii.parsing.validation

import org.scalatest.{Matchers, WordSpec}

class BracketsValidatorSpec extends WordSpec with Matchers {

  "BracketsValidator" should {
    "validate correct algebraic expression" in {
      val expr = "2 + 2 * x (2 * (3 * x + 2) + 2)"
      val res = Validator.bracketsValidator.validate(expr)

      res shouldBe Right(`expr`)
    }

    "find all errors during validation algebraic expression" in {
      val expr = "(2 + 2 * x (2 * (3 * x + 2) + 2)"
      val res = Validator.bracketsValidator.validate(expr)

      res shouldBe Left(List("Error in brackets position/number, index = 32"))
    }

    "find all errors during validation algebraic expression 2" in {
      val expr = "(2 + 2 * x)) ((2 * (3 * x + 2) + 2)"
      val res = Validator.bracketsValidator.validate(expr)

      res shouldBe Left(List("Error in brackets position/number, index = 11", "Error in brackets position/number, index = 35"))
    }
  }
}
