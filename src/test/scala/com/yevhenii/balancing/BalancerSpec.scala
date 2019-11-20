package com.yevhenii.balancing

import com.yevhenii.ExpressionOps
import com.yevhenii.parsing.FormulaParser
import org.scalatest.{Matchers, WordSpec}

class BalancerSpec extends WordSpec with Matchers {

  "Balancer.balance" should {
    "balance parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> "\t(\n\t\t\ta\n\t\t*\n\t\t\t(\n\t\t\t\t\tc\n\t\t\t\t+\n\t\t\t\t\td\n\t\t\t)\n\t)\n*\n\t(\n\t\t\tx\n\t\t-\n\t\t\t\ty\n\t\t\t*\n\t\t\t\t(\n\t\t\t\t\t\tz\n\t\t\t\t\t+\n\t\t\t\t\t\tk\n\t\t\t\t)\n\t)\n",
        "(a+b)*(c+d)" -> "\t(\n\t\t\ta\n\t\t+\n\t\t\tb\n\t)\n*\n\t(\n\t\t\tc\n\t\t+\n\t\t\td\n\t)\n",
        "(a*(c+d))*(x+y)" -> "\t(\n\t\t\ta\n\t\t*\n\t\t\t(\n\t\t\t\t\tc\n\t\t\t\t+\n\t\t\t\t\td\n\t\t\t)\n\t)\n*\n\t(\n\t\t\tx\n\t\t+\n\t\t\ty\n\t)\n",
        "(a+b+2)/(a-3*x)*123" -> "\t\t(\n\t\t\t\t\ta\n\t\t\t\t+\n\t\t\t\t\tb\n\t\t\t+\n\t\t\t\t2.0\n\t\t)\n\t/\n\t\t(\n\t\t\t\ta\n\t\t\t-\n\t\t\t\t\t3.0\n\t\t\t\t*\n\t\t\t\t\tx\n\t\t)\n*\n\t123.0\n",
        "(a+b)/(c+d+e+f)" -> "\t(\n\t\t\ta\n\t\t+\n\t\t\tb\n\t)\n/\n\t(\n\t\t\t\tc\n\t\t\t+\n\t\t\t\td\n\t\t+\n\t\t\t\te\n\t\t\t+\n\t\t\t\tf\n\t)\n",
        "(a+b)/(c+d+e+f)/g/h/i" -> "\t\t\t\t(\n\t\t\t\t\t\ta\n\t\t\t\t\t+\n\t\t\t\t\t\tb\n\t\t\t\t)\n\t\t\t/\n\t\t\t\t(\n\t\t\t\t\t\t\tc\n\t\t\t\t\t\t+\n\t\t\t\t\t\t\td\n\t\t\t\t\t+\n\t\t\t\t\t\t\te\n\t\t\t\t\t\t+\n\t\t\t\t\t\t\tf\n\t\t\t\t)\n\t\t/\n\t\t\tg\n\t/\n\t\th\n/\n\ti\n",
        "a/b/c/d/e/f" -> "\t\t\t\t\ta\n\t\t\t\t/\n\t\t\t\t\tb\n\t\t\t/\n\t\t\t\tc\n\t\t/\n\t\t\td\n\t/\n\t\te\n/\n\tf\n"
      )

      for ((expr, expected) <- expected) {
        FormulaParser(expr).map(Balancer.balance) match {
          case Left(e) =>
            throw e
          case Right(tree) =>
            ExpressionOps.asTreeShowable.show(tree) shouldBe expected
        }
      }
    }
  }
}
