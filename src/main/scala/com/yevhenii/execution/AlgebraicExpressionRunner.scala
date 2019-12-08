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
//    val resultFlow = expression.traverse(calculateNode)
    val resultFlow = buildFlow(expression)
    executor.init()
    val flowRes = Flow.run(executor)(resultFlow)
    interpretRes(flowRes).map(x => x -> executor.getDuration)
  }

  def buildFlow(x: Expression): Flow[Expression] = x match {
    case Number(_) => Flow.unit(x)
    case Constant(name) => Flow.unit(Number(context.constants.apply(name)))
    case BracketedExpression(expr) => buildFlow(expr)
    case binOperation @ BinOperation(left, op, right) =>
      val leftFlow = buildFlow(left)
      val rightFlow = buildFlow(right)
      Flow.map2withComplexity(leftFlow, rightFlow)(BinOperation(_, op, _), getOperationComplexity(op)).flatMap(calculate)
    case UnaryOperation(inner, UnaryOperator('-')) => Flow.unit(Number(-1))
      // todo check
      buildFlow(inner)
        .map(UnaryOperation(_, UnaryOperator('-')))
        .flatMap { case UnaryOperation(Number(n), UnaryOperator('-')) => Flow.unit(Number(-n)) }
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
      val func = getOperation(op)
      Flow.unit(Number(func(l, r)))
  }

  def getOperationComplexity(op: BinOperator): Int = op match {
    case BinOperator('+') => 1
    case BinOperator('-') => 1
    case BinOperator('*') => 2
    case BinOperator('/') => 4
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


/*


p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 4
p = <function1>, p2 = <function1>, complexity = 1
p = <function1>, p2 = <function1>, complexity = 2
res = 0.2571428571428571, time = 16, expression =
		3.0
	/
					2.0
				+
					3.0
			+
					4.0
				+
					5.0
		+
					6.0
				+
					7.0
			+
				8.0


*
		1.0
	+
		2.0


 */