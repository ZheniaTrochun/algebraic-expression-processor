package com.yevhenii.optimisation

import cats.{Id, Monad}
import com.yevhenii._
import com.yevhenii.ExpressionOps.expressionOps

import scala.annotation.tailrec

object Optimiser {

  def optimize(expr: Expression): Expression =
    expr.traverse(subtractReplace).traverse(divisionReplace)

  def subtractReplace(expr: Expression): Id[Expression] = Monad[Id].pure {
    expr match {
      case BinOperation(left, BinOperator('-'), right) =>
        BinOperation(left, BinOperator('+'), UnaryOperation(right, UnaryOperator('-')))
      case Constant(name) => Constant(new String(name.getBytes))
      case x => x
    }
  }

  def divisionReplace(expr: Expression): Id[Expression] = Monad[Id].pure {
    expr match {
      case BinOperation(left, BinOperator('/'), right) =>
        divisionReplaceLoop(left, right :: Nil)
      case Constant(name) => Constant(new String(name.getBytes))
      case x => x
    }
  }

  @tailrec
  private def divisionReplaceLoop(expr: Expression, stackExpr: List[Expression]): Expression = expr match {
    case BinOperation(left @ (Number(_) | Constant(_) | FuncCall(_, _) | BracketedExpression(_) | UnaryOperation(_, _)), BinOperator('/'), right) =>
      BinOperation(left, BinOperator('/'), join(right :: stackExpr))
    case BinOperation(left @ BinOperation(_, BinOperator('-') | BinOperator('+') | BinOperator('*'), _), BinOperator('/'), right) =>
      BinOperation(left, BinOperator('/'), join(right :: stackExpr))
    case BinOperation(left, BinOperator('/'), right) =>
      divisionReplaceLoop(left, right :: stackExpr)
    case x =>
      BinOperation(x, BinOperator('/'), join(stackExpr))
  }

  private def join(stackExpr: List[Expression]): Expression = {
    stackExpr.size match {
      case 0 =>
        Number(1)
      case 1 =>
        stackExpr.head
      case _ =>
        BracketedExpression(flatten(
          stackExpr.reduce((left, right) => BinOperation(left, BinOperator('*'), right))
        ))
    }
  }

  private def flatten(expression: Expression): Expression = expression match {
    case BinOperation(BracketedExpression(BinOperation(ll, BinOperator('*'), lr)), BinOperator('*'), right) =>
      BinOperation(BinOperation(flatten(ll), BinOperator('*'), flatten(lr)), BinOperator('*'), flatten(right))
    case BinOperation(left, BinOperator('*'), BracketedExpression(BinOperation(rl, BinOperator('*'), rr))) =>
      BinOperation(flatten(left), BinOperator('*'), BinOperation(flatten(rl), BinOperator('*'), flatten(rr)))

    case BracketedExpression(BinOperation(BracketedExpression(BinOperation(ll, BinOperator('*'), lr)), BinOperator('*'), right)) =>
      BinOperation(BinOperation(flatten(ll), BinOperator('*'), flatten(lr)), BinOperator('*'), flatten(right))
    case BracketedExpression(BinOperation(left, BinOperator('*'), BracketedExpression(BinOperation(rl, BinOperator('*'), rr)))) =>
      BinOperation(flatten(left), BinOperator('*'), BinOperation(flatten(rl), BinOperator('*'), flatten(rr)))

    case Constant(name) => Constant(new String(name.getBytes))
    case x => x
  }
}
