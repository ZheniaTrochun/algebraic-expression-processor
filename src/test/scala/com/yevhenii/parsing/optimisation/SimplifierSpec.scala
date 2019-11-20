package com.yevhenii.parsing.optimisation

import com.yevhenii.parsing.{Expression, FormulaParser}
import org.scalatest.{Matchers, WordSpec}

class SimplifierSpec extends WordSpec with Matchers {

  "Simplifier" should {
    "Simplifier.simplify() work on parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> "x*c*a+z*-(y)*c*a+k*-(y)*c*a+x*d*a+z*-(y)*d*a+k*-(y)*d*a",
        "(a+b)*(c+d)" -> "c*a+d*a+c*b+d*b",
        "(a*(c+d))*(x+y)" -> "x*c*a+y*c*a+x*d*a+y*d*a",
//        "(a+b+2)/(a-3*x)*123" -> "(a+b+2.0)/(a+-(3.0*x))*123.0", // todo
        "(a+b)/(c+d+e+f)" -> "a/(c+d+e+f)+b/(c+d+e+f)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "a/((c+d+e+f)*g*h*i)+b/((c+d+e+f)*g*h*i)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "a/(c*g*h*i+d*g*h*i+e*g*h*i+f*g*h*i)+b/(c*g*h*i+d*g*h*i+e*g*h*i+f*g*h*i)", // todo correct
        "a/b/c/d/e/f" -> "a/(b*c*d*e*f)"
      )

      for ((expr, expected) <- expected) {
        FormulaParser(expr).map(Optimiser.optimize).map(Simplifier.simplify) match {
          case Left(e) =>
            throw e
          case Right(tree) =>
//            println(s"$expr => ${Expression.asExpressionShowable.show(tree)}")
            Expression.asExpressionShowable.show(tree) shouldBe expected
        }
      }
    }
  }
}
