package com.yevhenii.parsing.balancing

import com.yevhenii.parsing.{Expression, FormulaParser}
import org.scalatest.{Matchers, WordSpec}

class BalancerSpec extends WordSpec with Matchers {

  "Balancer.balance" should {
    "balance parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> "\t(\n\t\t\ta\n\t\t*\n\t\t\t(\n\t\t\t\t\tc\n\t\t\t\t+\n\t\t\t\t\td\n\t\t\t)\n\t)\n*\n\t(\n\t\t\tx\n\t\t-\n\t\t\t\ty\n\t\t\t*\n\t\t\t\t(\n\t\t\t\t\t\tz\n\t\t\t\t\t+\n\t\t\t\t\t\tk\n\t\t\t\t)\n\t)",
        "(a+b)*(c+d)" -> "\t(\n\t\t\ta\n\t\t+\n\t\t\tb\n\t)\n*\n\t(\n\t\t\tc\n\t\t+\n\t\t\td\n\t)",
        "(a*(c+d))*(x+y)" -> "\t(\n\t\t\ta\n\t\t*\n\t\t\t(\n\t\t\t\t\tc\n\t\t\t\t+\n\t\t\t\t\td\n\t\t\t)\n\t)\n*\n\t(\n\t\t\tx\n\t\t+\n\t\t\ty\n\t)",
        "(a+b+2)/(a-3*x)*123" -> "\t\t(\n\t\t\t\t\ta\n\t\t\t\t+\n\t\t\t\t\tb\n\t\t\t+\n\t\t\t\t2.0\n\t\t)\n\t/\n\t\t(\n\t\t\t\ta\n\t\t\t-\n\t\t\t\t\t3.0\n\t\t\t\t*\n\t\t\t\t\tx\n\t\t)\n*\n\t123.0",
        "(a+b)/(c+d+e+f)" -> "\t(\n\t\t\ta\n\t\t+\n\t\t\tb\n\t)\n/\n\t(\n\t\t\t\tc\n\t\t\t+\n\t\t\t\td\n\t\t+\n\t\t\t\te\n\t\t\t+\n\t\t\t\tf\n\t)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "\t\t\t\t(\n\t\t\t\t\t\ta\n\t\t\t\t\t+\n\t\t\t\t\t\tb\n\t\t\t\t)\n\t\t\t/\n\t\t\t\t(\n\t\t\t\t\t\t\tc\n\t\t\t\t\t\t+\n\t\t\t\t\t\t\td\n\t\t\t\t\t+\n\t\t\t\t\t\t\te\n\t\t\t\t\t\t+\n\t\t\t\t\t\t\tf\n\t\t\t\t)\n\t\t/\n\t\t\tg\n\t/\n\t\th\n/\n\ti",
        "a/b/c/d/e/f" -> "\t\t\t\t\ta\n\t\t\t\t/\n\t\t\t\t\tb\n\t\t\t/\n\t\t\t\tc\n\t\t/\n\t\t\td\n\t/\n\t\te\n/\n\tf"
      )

      for ((expr, expected) <- expected) {
        FormulaParser(expr).map(Balancer.balance) match {
          case Left(e) =>
            throw e
          case Right(tree) =>
            Expression.asExpressionShowable.show(tree) shouldBe expected
        }
      }
    }
  }
}
