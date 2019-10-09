package com.yevhenii.parsing

import org.scalatest.{Matchers, WordSpec}

class FormulaParserSpec extends WordSpec with Matchers {

  "FormulaParser.parse" should {
    "parse correct expressions" in {
      val expressions = List(
        "1. + 2.0 * sin(pi / 2)",
        "2+2*2",
        "1+2*(3+4*5)",
        "8/2/2",
        "8-2-2"
      )

      expressions
        .map(FormulaParser.apply)
        .foreach(res => res.isRight shouldBe true)
    }

    "not parse incorrect expressions" in {
      val expressions = List(
        ".1. + 2.0 * sin(pi / 2)",
        "1+2+3+4**5",
        "1+2*((3+4*5)",
        "8/2/",
        "8-2-2)",
        "8-2(-2)",
        "8-2(+2)",
        "8-2+(2*)"
      )

      val results = expressions.map(FormulaParser.apply)

      for (res <- results) {
        res.isLeft shouldBe true
      }
    }
  }
}
