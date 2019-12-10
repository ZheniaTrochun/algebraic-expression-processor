package com.yevhenii.execution

import com.yevhenii._
import com.yevhenii.ExpressionOps._
import com.yevhenii.execution.Execution.Flow
import com.yevhenii.execution.Execution.Flow._
import com.yevhenii.execution.Executor.DataFlowExecutor

import scala.annotation.tailrec

// todo (1+2)*(3/(2+3+4+5+6+7+8))
object AlgebraicExpressionRunner {

  type Result = (Double, Int)
  type Err = String

  def run(expression: Expression)(implicit context: Context): Either[Err, Result] = {
    val executor = new DataFlowExecutor(context)
    val resultFlow = buildFlow(expression)

    // todo move run inside executor ????
    executor.init()
    val flowRes = Flow.run(executor)(resultFlow)
    executor.stop()

    interpretRes(flowRes).map(x => x -> executor.getDuration)
  }

  def buildFlow(x: Expression)(implicit context: Context): Flow[Expression] = x match {
    case Number(_) => Flow.unit(x)
    case Constant(name) => Flow.unit(Number(context.constants.apply(name)))
    case BracketedExpression(expr) => buildFlow(expr)
    case binOperation @ BinOperation(left, op, right) =>
      val leftFlow = buildFlow(left)
      val rightFlow = buildFlow(right)
      Flow.map2withComplexity(leftFlow, rightFlow)((l: Expression, r: Expression) => {
        println(s"[${System.currentTimeMillis()}] l = $l, r = $r")
        BinOperation(l, op, r)
      }, op.complexity).flatMap(calculate)
    case UnaryOperation(inner, UnaryOperator('-')) =>
      // todo check
      buildFlow(inner)
        .map(UnaryOperation(_, UnaryOperator('-')))
        .flatMap { case UnaryOperation(Number(n), UnaryOperator('-')) => Flow.unit(Number(-n)) }
  }

  @tailrec
  final def calculateNode(x: Expression)(implicit context: Context): Flow[Expression] = x match {
    case Number(_) => Flow.unit(x)
    case Constant(name) => Flow.unit(Number(context.constants.apply(name)))
    case BracketedExpression(expr) => calculateNode(expr)
    case binOperation: BinOperation => calculate(binOperation)
    case UnaryOperation(Number(n), UnaryOperator('-')) => Flow.unit(Number(-n))
  }

  def calculate(binOperation: BinOperation): Flow[Expression] = binOperation match {
    case BinOperation(Number(l), op, Number(r)) =>
      val func = getOperation(op)
      Flow.unit(Number(func(l, r)))
  }

  def getOperation(op: BinOperator): (Double, Double) => Double = op match {
    case BinOperator('+') => _ + _
    case BinOperator('-') => _ - _
    case BinOperator('*') => _ * _
    case BinOperator('/') => _ / _
  }

  def interpretRes(tree: Expression): Either[Err, Double] = tree match {
    case Number(x) => Right(x)
    case other: Expression => Left(s"Expected Number but got: ${asExpressionShowable.show(other)}")
  }
}
