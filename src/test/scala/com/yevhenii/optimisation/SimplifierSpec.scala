package com.yevhenii.optimisation

import com.yevhenii.ExpressionOps
import com.yevhenii.parsing.FormulaParser
import org.scalatest.{Matchers, WordSpec}

class SimplifierSpec extends WordSpec with Matchers {

  "Simplifier" should {
    "Simplifier.simplify() work on parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> "x*a*c+z*a*c*-(y)+k*a*c*-(y)+x*a*d+z*a*d*-(y)+k*a*d*-(y)",
        "(a+b)*(c+d)" -> "c*a+d*a+c*b+d*b",
        "(a*(c+d))*(x+y)" -> "x*a*c+y*a*c+x*a*d+y*a*d",
        "(a+b+2)/(a-3*x)*123" -> "a*123.0/(a+-3.0*x)+b*123.0/(a+-3.0*x)+2.0*123.0/(a+-3.0*x)",
        "(a+b)/(c+d+e+f)" -> "a/(c+d+e+f)+b/(c+d+e+f)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "a/((c+d+e+f)*g*h*i)+b/((c+d+e+f)*g*h*i)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "a/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)+b/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)",
        "a/b/c/d/e/f" -> "a/(b*c*d*e*f)"
      )

      for ((expr, expected) <- expected) {
        FormulaParser(expr).map(Optimiser.optimize).map(Simplifier.simplify) match {
          case Left(e) =>
            throw e
          case Right(tree) =>
            ExpressionOps.asExpressionShowable.show(tree) shouldBe expected
        }
      }
    }
  }
}
