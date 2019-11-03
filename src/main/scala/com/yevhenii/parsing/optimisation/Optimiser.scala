package com.yevhenii.parsing.optimisation

import com.yevhenii.parsing.{BinOperation, BinOperator, BracketedExpression, Constant, Expression, FuncCall, Number}

// todo DOES NOT WORK EVEN CLOSE
object Optimiser {

  def divisionReplaceLoop(expr: Expression): Expression = expr match {
    case BinOperation(left, BinOperator("/"), right) => BinOperation(left, BinOperator("*"), divisionReplaceLoop(right))
    case x => x
  }

  def optimize(expr: Expression): Expression = expr match {
    case BinOperation(left, BinOperator("/"), right) => BinOperation(optimize(left), BinOperator("/"), BracketedExpression(divisionReplaceLoop(right)))
    case BinOperation(left, op, right) => BinOperation(optimize(left), op, optimize(right))
    case FuncCall(name, inner) => FuncCall(name, optimize(inner))
    case BracketedExpression(inner) => BracketedExpression(optimize(inner))
    case x => x
  }
}
