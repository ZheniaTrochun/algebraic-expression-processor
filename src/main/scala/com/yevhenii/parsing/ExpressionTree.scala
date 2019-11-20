package com.yevhenii.parsing

sealed trait Expression

sealed trait Operator
case class BinOperator(operator: Char) extends Operator
case class UnaryOperator(operator: Char) extends Operator

case class Number(value: Double) extends Expression
case class Constant(name: String) extends Expression
case class BinOperation(left: Expression, operator: BinOperator, right: Expression) extends Expression
case class UnaryOperation(expr: Expression, op: UnaryOperator) extends Expression
case class FuncCall(funcName: Constant, argument: Expression) extends Expression
case class BracketedExpression(expr: Expression) extends Expression
