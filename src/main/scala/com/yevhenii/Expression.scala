package com.yevhenii

sealed trait Expression
sealed trait ExpressionTreeLeaf

sealed trait Operator

case class BinOperator(operator: Char) extends Operator {
  val complexity: Int = operator match {
    case '+' => 1
    case '-' => 1
    case '*' => 2
    case '/' => 4
    case _   => 0
  }
}

case class UnaryOperator(operator: Char) extends Operator

case class Number(value: Double) extends Expression with ExpressionTreeLeaf
case class Constant(name: String) extends Expression with ExpressionTreeLeaf
case class BinOperation(left: Expression, operator: BinOperator, right: Expression) extends Expression
case class UnaryOperation(expr: Expression, op: UnaryOperator) extends Expression
case class FuncCall(funcName: Constant, argument: Expression) extends Expression
case class BracketedExpression(expr: Expression) extends Expression
