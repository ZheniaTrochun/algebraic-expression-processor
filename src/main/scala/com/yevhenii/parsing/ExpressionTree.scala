package com.yevhenii.parsing

import cats.Show

sealed trait Expression

case class BinOperator(operator: String)
case class UnaryOperator(operator: String)

case class Number(value: Double) extends Expression
case class Constant(name: String) extends Expression
case class BinOperation(left: Expression, operator: BinOperator, right: Expression) extends Expression
case class FuncCall(funcName: Constant, argument: Expression) extends Expression
case class BracketedExpression(expr: Expression) extends Expression
case class UnaryOperation(expr: Expression, op: UnaryOperator) extends Expression

object Expression {
  implicit val showable: Show[Expression] = new Show[Expression] {
    def showLoop(x: Expression, n: Int): String = x match {
      case Number(value) =>
        tabs(n) + s"$value\n"
      case Constant(name) =>
        tabs(n) + s"$name\n"
      case BinOperation(left, BinOperator(op), right) =>
        showLoop(left, n + 1) + tabs(n) +  op + "\n" + showLoop(right, n + 1)
      case FuncCall(Constant(name), arg) =>
        tabs(n) + s"$name(\n${showLoop(arg, n + 1)}" + tabs(n) + ")\n"
      case BracketedExpression(expr) =>
//        tabs(n) + s"(\n${showLoop(expr, n + 1)}" + tabs(n) + ")\n"
        showLoop(expr, n + 1)
      case UnaryOperation(expr, UnaryOperator(op)) =>
        tabs(n) + s"$op(\n${showLoop(expr, n + 1)}" + tabs(n) + ")\n"
    }

    def tabs(n: Int): String = "\t" * n

    override def show(x: Expression): String = showLoop(x, 0)
  }

  implicit class expressionShowable(x: Expression) {
    def show(): String = Show[Expression].show(x)
  }
}