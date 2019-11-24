package com.yevhenii.optimisation

import cats.{Id, Monad}
import com.yevhenii._
import com.yevhenii.ExpressionOps._

// todo just DRAFT
object CommutativityOptimizer {

  def shuffle(expression: Expression): Expression = {
    expression.traverse(sortByComplexity)
  }

  private def sortByComplexity(expression: Expression): Id[Expression] = Monad[Id].pure {
    expression match {
      case original @ BinOperation(left, op @ BinOperator('+' | '*'), right) =>
        val leftWeight = evaluate(left)
        val rightWeight = evaluate(right)
        if (rightWeight > leftWeight) BinOperation(right, op, left)
        else original

      case x => x
    }
  }

  private def evaluate(expression: Expression): Int = expression match {
    case BinOperation(left, BinOperator('+'), right) => 1 + evaluate(left) + evaluate(right)
    case BinOperation(left, BinOperator('_'), right) => 1 + evaluate(left) + evaluate(right)
    case BinOperation(left, BinOperator('*'), right) => 2 + evaluate(left) + evaluate(right)
    case BinOperation(left, BinOperator('/'), right) => 4 + evaluate(left) + evaluate(right)
    case FuncCall(_, inner) => 1 + evaluate(inner)
    case BracketedExpression(inner) => evaluate(inner)
    case UnaryOperation(inner, _) => evaluate(inner)
  }
}
