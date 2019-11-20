package com.yevhenii.parsing.optimisation

import com.yevhenii.parsing.{ExpressionOps, FormulaParser}
import org.scalatest.{Matchers, WordSpec}

class OptimiserSpec extends WordSpec with Matchers {

  "Optimizer.optimize" should {
    "optimize parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> "(a*(c+d))*(x+-(y*(z+k)))",
        "(a+b)*(c+d)" -> "(a+b)*(c+d)",
        "(a*(c+d))*(x+y)" -> "(a*(c+d))*(x+y)",
        "(a+b+2)/(a-3*x)*123" -> "(a+b+2.0)/(a+-(3.0*x))*123.0",
        "(a+b)/(c+d+e+f)" -> "(a+b)/(c+d+e+f)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "(a+b)/((c+d+e+f)*g*h*i)",
        "a/b/c/d/e/f" -> "a/(b*c*d*e*f)"
      )

      for ((expr, expected) <- expected) {
        FormulaParser(expr).map(Optimiser.optimize) match {
          case Left(e) =>
            throw e
          case Right(tree) =>
            ExpressionOps.asExpressionShowable.show(tree) shouldBe expected
        }
      }
    }
  }
}
