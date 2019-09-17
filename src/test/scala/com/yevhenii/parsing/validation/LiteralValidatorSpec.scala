package com.yevhenii.parsing.validation

import org.scalatest.{Matchers, WordSpec}

class LiteralValidatorSpec extends WordSpec with Matchers {

  "LiteralValidator" should {
    "validate correct algebraic expression" in {
      val expr = "2 + 2 * x (2 * (3 * x + 2) + 2 - ln(10))"
      val res = Validator.literalsValidatorCreator(Set("x", "ln")).validate(expr)

      res shouldBe Right(`expr`)
    }

    "find all errors during validation algebraic expression" in {
      val expr = "2 + 2 * x (2 * (3 * x + 2) + 2)"
      val res = Validator.literalsValidatorCreator(Set("y")).validate(expr)

      res shouldBe Left(List("Found unexpected literal, literal = y"))
    }

    "find all errors during validation algebraic expression 2" in {
      val expr = "(2 + 2 * x) * (2 * sin(3 * x + 2) + 2)"
      val res = Validator.literalsValidatorCreator(Set()).validate(expr)

      res shouldBe Left(List("Found unexpected literal, literal = x", "Found unexpected literal, literal = sin"))
    }
  }
}
