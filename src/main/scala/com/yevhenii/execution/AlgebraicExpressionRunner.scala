package com.yevhenii.execution

import com.yevhenii._
import com.yevhenii.ExpressionOps._
import com.yevhenii.execution.Execution.Flow
import com.yevhenii.execution.Execution.Flow._
import com.yevhenii.execution.Executor.DataFlowExecutor

import scala.annotation.tailrec


// todo (1+2)*(3/(2+3+4+5+6+7+8))
class AlgebraicExpressionRunner(context: Context) {

  type Result = (Double, Int)
  type Err = String

  val executor = new DataFlowExecutor(context)

  def run(expression: Expression): Either[Err, Result] = {
    val resultFlow = expression.traverse(calculateNode)
    executor.init()
    val flowRes = Flow.run(executor)(resultFlow)
    interpretRes(flowRes).map(x => x -> executor.getDuration)
  }

  @tailrec
  final def calculateNode(x: Expression): Flow[Expression] = x match {
    case Number(_) => Flow.unit(x)
    case Constant(name) => Flow.unit(Number(context.constants.apply(name)))
    case BracketedExpression(expr) => calculateNode(expr)
    case binOperation: BinOperation => calculate(binOperation)
    case UnaryOperation(Number(n), UnaryOperator('-')) => Flow.unit(Number(-n))
  }

  def calculate(binOperation: BinOperation): Flow[Expression] = binOperation match {
    case BinOperation(Number(l), op, Number(r)) =>
      val (func, complexity) = getOperation(op)
      Flow.unit(Number(func(l, r)), complexity)
  }

  def getOperation(op: BinOperator): ((Double, Double) => Double, Int) = op match {
    case BinOperator('+') => ((x: Double, y: Double) => x + y) -> 1
    case BinOperator('-') => ((x: Double, y: Double) => x - y) -> 1
    case BinOperator('*') => ((x: Double, y: Double) => x * y) -> 2
    case BinOperator('/') => ((x: Double, y: Double) => x / y) -> 4
  }

  def interpretRes(tree: Expression): Either[Err, Double] = tree match {
    case Number(x) => Right(x)
    case other: Expression => Left(s"Expected Number but got: ${asExpressionShowable.show(other)}")
  }
}
