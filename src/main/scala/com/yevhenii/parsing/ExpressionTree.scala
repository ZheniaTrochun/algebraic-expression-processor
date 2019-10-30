package com.yevhenii.parsing

import cats.Show

sealed trait Expression

case class BinOperator(operator: String)

case class Number(value: Double) extends Expression
case class Constant(name: String) extends Expression
case class BinOperation(left: Expression, operator: BinOperator, right: Expression) extends Expression
case class FuncCall(funcName: Constant, argument: Expression) extends Expression

object Expression {
  implicit val showable: Show[Expression] = new Show[Expression] {
    def showLoop(x: Expression, tabs: Int): String = x match {
      case Number(value) => "\t" * tabs + s"$value\n"
      case Constant(name) => "\t" * tabs + s"$name\n"
      case BinOperation(left, BinOperator(op), right) => showLoop(left, tabs + 1) + "\t" * tabs +  op + "\n" + showLoop(right, tabs + 1)
      case FuncCall(Constant(name), arg) => "\t" * tabs + s"$name(\n${showLoop(arg, tabs + 1)}" + "\t" * tabs + ")"
    }
    override def show(x: Expression): String = showLoop(x, 0)
  }

  implicit class expressionShowable(x: Expression) {
    def show(): String = Show[Expression].show(x)
  }
}