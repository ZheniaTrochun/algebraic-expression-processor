package com.yevhenii.optimisation

import com.yevhenii.ExpressionOps
import com.yevhenii.parsing.FormulaParser
import org.scalatest.{Matchers, WordSpec}

class SimplifierSpec extends WordSpec with Matchers {

  "Simplifier" should {
    ".simplify() work on parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> "x*a*c+z*a*c*-(y)+k*a*c*-(y)+x*a*d+z*a*d*-(y)+k*a*d*-(y)",
        "(a+b)*(c+d)" -> "c*a+d*a+c*b+d*b",
        "(a*(c+d))*(x+y)" -> "x*a*c+y*a*c+x*a*d+y*a*d",
        "(a+b+2)/(a-3*x)*123" -> "a*123.0/(a+-3.0*x)+b*123.0/(a+-3.0*x)+2.0*123.0/(a+-3.0*x)",
        "(a+b)/(c+d+e+f)" -> "a/(c+d+e+f)+b/(c+d+e+f)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "a/((c+d+e+f)*g*h*i)+b/((c+d+e+f)*g*h*i)",
        "(a+b)/(c+d+e+f)/g/h/i" -> "a/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)+b/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)",
        "a/b/c/d/e/f" -> "a/(b*c*d*e*f)",
        "m-(a-d*(f-k))+(b-10)/(z+b)+(e-g)*(y-2)" -> "m+-(a)+-(f)*-(d)+k*-(d)+b/(z+b)+-10.0/(z+b)+y*e+-2.0*e+y*-(g)+-2.0*-(g)"
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

    ".simplifyOneByOne() work on parsed expressions correctly" in {
      val expected = Map(
        "(a*(c+d))*(x-y*(z+k))" -> List("x*a*c+z*a*c*-(y)+k*a*c*-(y)+x*a*d+z*a*d*-(y)+k*a*d*-(y)", "x*a*c+z*a*c*-(y)+k*a*c*-(y)+(x*d+z*d*-(y)+k*d*-(y))*a", "x*a*c+z*a*c*-(y)+k*a*c*-(y)+d*(x+z*-(y)+k*-(y))*a", "(x*c+z*c*-(y)+k*c*-(y))*a+d*(x+z*-(y)+k*-(y))*a", "c*(x+z*-(y)+k*-(y))*a+d*(x+z*-(y)+k*-(y))*a", "(c*a+d*a)*(x+z*-(y)+k*-(y))", "(c*a+d*a)*(x+-(y)*(z+k))", "(a*(c+d))*(x+-(y*(z+k)))"),
        "(a+b)*(c+d)" -> List("c*a+d*a+c*b+d*b", "c*a+d*a+b*(c+d)", "a*(c+d)+b*(c+d)", "(a+b)*(c+d)"),
        "(a*(c+d))*(x+y)" -> List("x*a*c+y*a*c+x*a*d+y*a*d", "x*a*c+y*a*c+(x*d+y*d)*a", "x*a*c+y*a*c+d*(x+y)*a", "(x*c+y*c)*a+d*(x+y)*a", "c*(x+y)*a+d*(x+y)*a", "(c*a+d*a)*(x+y)", "(a*(c+d))*(x+y)"),
        "(a+b+2)/(a-3*x)*123" -> List("a*123.0/(a+-3.0*x)+b*123.0/(a+-3.0*x)+2.0*123.0/(a+-3.0*x)", "(a/(a+-3.0*x)+b/(a+-3.0*x)+2.0/(a+-3.0*x))*123.0", "(a+b+2.0)/(a+-(3.0*x))*123.0"),
        "(a+b)/(c+d+e+f)" -> List("a/(c+d+e+f)+b/(c+d+e+f)", "(a+b)/(c+d+e+f)"),
        "(a+b)/(c+d+e+f)/g/h/i" -> List("a/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)+b/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)", "(a+b)/(c*i*h*g+d*i*h*g+e*i*h*g+f*i*h*g)", "(a+b)/((c*h*g+d*h*g+e*h*g+f*h*g)*i)", "(a+b)/((c*g+d*g+e*g+f*g)*h*i)", "(a+b)/((c+d+e+f)*g*h*i)"),
        "a/b/c/d/e/f" -> List("a/(b*c*d*e*f)"),
        "m-(a-d*(f-k))+(b-10)/(z+b)+(e-g)*(y-2)" -> List("m+-(a)+-(f)*-(d)+k*-(d)+b/(z+b)+-10.0/(z+b)+y*e+-2.0*e+y*-(g)+-2.0*-(g)", "m+-(a)+-(f)*-(d)+k*-(d)+b/(z+b)+-(10.0/(z+b))+y*e+-2.0*e+y*-(g)+-2.0*-(g)", "m+-(a)+-(f)*-(d)+k*-(d)+(b+-(10.0))/(z+b)+y*e+-2.0*e+y*-(g)+-(2.0*-(g))", "m+-(a)+-(f)*-(d)+k*-(d)+(b+-(10.0))/(z+b)+y*e+-(2.0*e)+-(g)*(y+-(2.0))", "m+-(a)+-(f)*-(d)+k*-(d)+(b+-(10.0))/(z+b)+e*(y+-(2.0))+-(g*(y+-(2.0)))", "m+-(a+f*-(d)+-(k*-(d)))+(b+-(10.0))/(z+b)+(e+-(g))*(y+-(2.0))", "m+-((a+-(d*(f+-(k)))))+(b+-(10.0))/(z+b)+(e+-(g))*(y+-(2.0))")
      )

      for ((expr, expected) <- expected) {
        FormulaParser(expr).map(Optimiser.optimize).map(Simplifier.simplifyOneByOne) match {
          case Left(e) =>
            throw e
          case Right(list) =>
            list.map(ExpressionOps.asExpressionShowable.show) should contain theSameElementsAs expected
        }
      }
    }
  }
}
