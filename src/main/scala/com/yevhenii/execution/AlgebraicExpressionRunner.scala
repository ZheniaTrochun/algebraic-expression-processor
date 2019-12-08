package com.yevhenii.execution

import cats.Monad
import cats.instances.try_._
import com.yevhenii._
import com.yevhenii.ExpressionOps._
//import com.yevhenii.execution.AsyncFlowImpl.Flow
//import com.yevhenii.execution.AsyncFlowImpl._
import com.yevhenii.execution.Execution.Flow
import com.yevhenii.execution.Execution.Flow._
import com.yevhenii.execution.Executor.DataFlowExecutor

import scala.annotation.tailrec
import scala.util.Try

//class AlgebraicExpressionRunner(context: Context) {
//
//  type Result = (Double, Int)
//  type Err = String
//
//  val executor = DataFlowExecutor
//
//  implicit val flowMonad = new Monad[Flow] {
//    override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = AsyncFlowImpl.flatMap(fa)(f)(executor)
//
//    override def tailRecM[A, B](a: A)(f: A => Flow[Either[A, B]]): Flow[B] = ???
//
//    override def pure[A](x: A): Flow[A] = AsyncFlowImpl.pure(x)
//  }
//
//  def run(expression: Expression): Either[Err, Result] = {
//    executor.init()
//    val resultFlow = expression.traverse(calculateNode)
//    val flowRes = resultFlow.get()(executor)
//    interpretRes(flowRes).map(x => x -> executor.getDuration)
//  }
//
//  // todo move context somewhere
//  @tailrec
//  final def calculateNode(x: Expression): Flow[Expression] = x match {
//    case Number(_) => pure(x)
//    case Constant(name) => pure(Number(context.constants.apply(name)))
//    case BracketedExpression(expr) => calculateNode(expr)
//    case binOperation: BinOperation => calculate(binOperation)
//    case UnaryOperation(Number(n), UnaryOperator('-')) => pure(Number(-n))
//  }
//
//  def calculate(binOperation: BinOperation): Flow[Expression] = binOperation match {
//    case BinOperation(Number(l), op, Number(r)) => AsyncFlow(() => Number(getOperation(op)(l, r))) // todo try pure
//  }
//
//  def getOperation(op: BinOperator): (Double, Double) => Double = op match {
//    case BinOperator('+') => _ + _
//    case BinOperator('-') => _ - _
//    case BinOperator('*') => _ * _
//    case BinOperator('/') => _ / _
//  }
//
//  def interpretRes(tree: Expression): Either[Err, Double] = tree match {
//    case Number(x) => Right(x)
//    case other: Expression => Left(s"Expected Number but got: ${asExpressionShowable.show(other)}")
//  }
//
//  // todo fix
////  implicit val flowEitherMonad = flowMonad.compose[Try]
//}



class AlgebraicExpressionRunner(context: Context) {

  type Result = (Double, Int)
  type Err = String

  val executor = DataFlowExecutor

  def run(expression: Expression): Either[Err, Result] = {
    val resultFlow = expression.traverse(calculateNode)
    executor.init()
    val flowRes = Flow.run(executor)(resultFlow)
    interpretRes(flowRes).map(x => x -> executor.getDuration)
  }

  // todo move context somewhere
  @tailrec
  final def calculateNode(x: Expression): Flow[Expression] = x match {
    case Number(_) => Flow.unit(x)
    case Constant(name) => Flow.unit(Number(context.constants.apply(name)))
    case BracketedExpression(expr) => calculateNode(expr)
    case binOperation: BinOperation => calculate(binOperation)
    case UnaryOperation(Number(n), UnaryOperator('-')) => Flow.unit(Number(-n))
  }

  def calculate(binOperation: BinOperation): Flow[Expression] = binOperation match {
    case BinOperation(Number(l), op, Number(r)) => Flow.unit(Number(getOperation(op)(l, r))) // todo maybe unit will be fine
//    case BinOperation(Number(l), op, Number(r)) => Flow.lazyUnit(Number(getOperation(op)(l, r))) // todo maybe unit will be fine
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

  // todo fix
//  implicit val flowEitherMonad = flowMonad.compose[Try]
}
