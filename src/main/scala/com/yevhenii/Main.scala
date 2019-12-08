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

object Main extends IOApp {

  val prepareEnv: IO[Unit] = IO {
    // todo does not work, deletes everything
//    Visualizer.Directory.toFile.listFiles().foreach(_.delete())
  }

  def executeAndPrint(expression: Expression): Unit = { //: Either[String, (Double, Int)] = {
    val context = Context(10, Map(), Map())
    val runer = new AlgebraicExpressionRunner(context)
    val res = runer.run(expression)
    res match {
      case Left(v) => sys.error(v)
      case Right((num, time)) => println(s"res = $num, time = $time, expression = \n${asTreeShowable.show(expression)}")
    }
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
