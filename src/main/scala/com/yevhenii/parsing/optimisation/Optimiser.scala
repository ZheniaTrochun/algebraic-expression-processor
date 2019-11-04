package com.yevhenii.parsing.optimisation

import com.yevhenii.parsing.{BinOperation, BinOperator, BracketedExpression, Constant, Expression, FormulaParser, FuncCall, Number, UnaryOperation, UnaryOperator}

import scala.annotation.tailrec

object Optimiser {

  def subtractReplace(expr: Expression): Expression = expr match {
    case BinOperation(left, BinOperator("-"), right) =>
      BinOperation(left, BinOperator("+"), UnaryOperation(subtractReplace(right), UnaryOperator("-")))
    case x => x
  }

  def optimize(expr: Expression): Expression = expr match {
    case x @ BinOperation(_, BinOperator("-"), _) => subtractReplace(x)
    case BinOperation(left, BinOperator("/"), right) => divisionReplaceLoop(left, right :: Nil)
    case BinOperation(left, op, right) => BinOperation(optimize(left), op, optimize(right))
    case FuncCall(name, inner) => FuncCall(name, optimize(inner))
    case BracketedExpression(inner) => BracketedExpression(optimize(inner))
    case UnaryOperation(inner, op) => UnaryOperation(optimize(inner), op)
    case x => x
  }

  @tailrec
  def divisionReplaceLoop(expr: Expression, stackExpr: List[Expression]): Expression = expr match {
    case BinOperation(left @ (Number(_) | Constant(_) | FuncCall(_, _) | BracketedExpression(_) | UnaryOperation(_, _)), BinOperator("/"), right) =>
      BinOperation(left, BinOperator("/"), join(right :: stackExpr))
    case BinOperation(left @ BinOperation(_, BinOperator("-") | BinOperator("+") | BinOperator("*"), _), BinOperator("/"), right) =>
      BinOperation(left, BinOperator("/"), join(right :: stackExpr))
    case BinOperation(left, BinOperator("/"), right) =>
      divisionReplaceLoop(left, right :: stackExpr)
    case x => BinOperation(x, BinOperator("/"), join(stackExpr))
  }

  def join(stackExpr: List[Expression]): Expression = {
    stackExpr.size match {
      case 0 =>
        Number(1)
      case 1 =>
        stackExpr.head
      case _ =>
        BracketedExpression(stackExpr.reduce((left, right) => BinOperation(left, BinOperator("*"), right)))
    }
  }
}
