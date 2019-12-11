package com.yevhenii.execution

import cats.data.Writer
import com.typesafe.scalalogging.LazyLogging
import com.yevhenii._
import com.yevhenii.ExpressionOps._
import com.yevhenii.execution.Execution.Flow
import com.yevhenii.execution.Execution.Flow._
import com.yevhenii.execution.Executor.DataFlowExecutor

import scala.annotation.tailrec

// todo (1+2)*(3/(2+3+4+5+6+7+8))
object AlgebraicExpressionRunner extends LazyLogging {

  case class ExecutionResult(expression: Expression, value: Double, time: Int, speedup: Double, efficiency: Double)

  type Err = String
  type Log = List[String]

  def run(expression: Expression)(implicit context: Context): Writer[Log, Either[Err, ExecutionResult]] = {
    val executor = new DataFlowExecutor(context)
    val resultFlow = buildFlow(expression)

    val flowRes = executor.run(resultFlow)

    flowRes.map(_.flatMap { case (res, time) =>
      val interpreted = interpretRes(res)
      interpreted.map { x =>
        val speedup = calculateSpeedup(expression, time)
        val efficiency = calculateEfficiency(speedup, context.parallelism)
        ExecutionResult(expression, x, time, speedup, efficiency)
      }
    }).mapWritten(str => str :: Nil)
  }

  def buildFlow(x: Expression)(implicit context: Context): Flow[Expression] = x match {
    case Number(_) => Flow.unit(x)
    case Constant(name) => Flow.unit(Number(context.constants.apply(name)))
    case BracketedExpression(expr) => buildFlow(expr)

    case BinOperation(left, op, right) =>
      val leftFlow = buildFlow(left)
      val rightFlow = buildFlow(right)
      Flow.map2withComplexity(leftFlow, rightFlow)(
        (l: Expression, r: Expression) => {
          logger.info(s"executing: $l ${op.operator} $r")
          BinOperation(l, op, r)
        },
        op.complexity
      )
        .flatMap(calculate)

    case UnaryOperation(inner, UnaryOperator('-')) =>
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

  def getLinearTime(expression: Expression): Int = expression match {
    case BinOperation(left, operator, right) => getLinearTime(left) + getLinearTime(right) + operator.complexity
    case UnaryOperation(inner, _) => getLinearTime(inner)
    case BracketedExpression(inner) => getLinearTime(inner)
    case FuncCall(_, inner) => getLinearTime(inner) + 1
    case _ => 0
  }

  def calculateSpeedup(expression: Expression, time: Int): Double = {
    val linearTime = getLinearTime(expression)
    linearTime / time.toDouble
  }

  def calculateEfficiency(speedup: Double, parallelism: Int): Double = speedup / parallelism
}
