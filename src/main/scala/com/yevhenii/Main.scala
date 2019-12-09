package com.yevhenii

import cats.Show
import cats.effect._
import cats.implicits._
import com.yevhenii.visualization.Visualizer._
import com.yevhenii.balancing.Balancer._
import com.yevhenii.execution.{AlgebraicExpressionRunner, Context}
import com.yevhenii.optimisation.Optimiser._
import com.yevhenii.optimisation.Simplifier._
import com.yevhenii.optimisation.CommutativityOptimizer._
import com.yevhenii.parsing.FormulaParser
import com.yevhenii.parsing.FormulaParser.ParseError._
import com.yevhenii.ExpressionOps._
import com.yevhenii.utils.IoUtils._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

// todo good example:
// (a/(a*b+a*b))+(a*(b*d+b*e+b+e))

// todo 1+2+3+4+5 => 2 cores => time 4 => INCORRECT => should be calculated from left to right
object Main extends IOApp {

  val prepareEnv: IO[Unit] = IO {
    // todo does not work, deletes everything
//    Visualizer.Directory.toFile.listFiles().foreach(_.delete())
  }

  val context = Context(2, 100, c => c.head.toDouble, _ => x => x * x)

  def executeAndPrint(expression: Expression): Unit = { //: Either[String, (Double, Int)] = {
    val runner = new AlgebraicExpressionRunner(context)
    val res = runner.run(expression)
    res match {
      case Left(v) => sys.error(v)
      case Right((num, time)) =>
        val speedup = calculateSpeedup(expression, time)
        val efficiency = calculateEfficiency(speedup)
        println(s"res = $num, time = $time, speedup = $speedup, efficiency = $efficiency, expression = \n${asTreeShowable.show(expression)}")
    }
  }

  // todo move somewhere
  def getLinearTime(expression: Expression): Int = expression match {
    case BinOperation(left, operator, right) => getLinearTime(left) + getLinearTime(right) + getOperationComplexity(operator)
    case UnaryOperation(inner, operator) => getLinearTime(inner)
    case BracketedExpression(inner) => getLinearTime(inner)
    case FuncCall(name, inner) => getLinearTime(inner) + 1
    case _ => 0
  }

  def getOperationComplexity(op: BinOperator): Int = op match {
    case BinOperator('+') => 1
    case BinOperator('-') => 1
    case BinOperator('*') => 2
    case BinOperator('/') => 4
  }

  def calculateSpeedup(expression: Expression, time: Int): Double = {
    val linearTime = getLinearTime(expression)
    linearTime / time.toDouble
  }

  def calculateEfficiency(speedup: Double): Double = {
    speedup / context.parallelism
  }

  val getInput: IO[String] = IO {
    println("Enter expression:")
    io.StdIn.readLine()
  }

  def printFailure(e: Throwable): IO[ExitCode] = IO {
    println("Invalid expression:")
    println(Show[Throwable].show(e))
  }.as(ExitCode.Error)

  def printSuccess(x: List[Expression]): IO[ExitCode] = IO {
    println("Processed successfully")
  }.as(ExitCode.Success)

  def visualizeItemUnsafe(pair: (Expression, Int)): Future[Unit] = {
    val (item, index) = pair
    visualize(index.toString)(item).unsafeToFuture()
  }

  def visualizeResults(list: List[Expression]): IO[List[Unit]] =
    IO.fromFuture(IO(Future.traverse(list.zipWithIndex)(visualizeItemUnsafe)))

  val expressionsIO: IO[List[Expression]] =
    prepareEnv
      .flatMap(_ => getInput)
      .map(FormulaParser.apply)
      .flatMap(IO.fromEither)
      .map(optimize)
      .map(simplifyOneByOne)
      .map(_.map(balance))
      .map(_.flatMap(orderByComplexity))
      .peek(visualizeResults)
      .peek(x => IO(x.foreach(executeAndPrint)))

  def runOnce(): IO[ExitCode] = expressionsIO.redeemWith(printFailure, printSuccess)

  def runAsRepl(): IO[ExitCode] = IO {
    while (true) {
      runOnce().unsafeRunSync()
    }
  }.as(ExitCode.Success)

  def run(args: List[String]): IO[ExitCode] = {
    if (args.contains("repl")) runAsRepl()
    else runOnce()
  }
}
