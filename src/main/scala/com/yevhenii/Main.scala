package com.yevhenii

import cats.Show
import cats.data.Writer
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
import com.yevhenii.execution.AlgebraicExpressionRunner.ExecutionResult
import com.yevhenii.utils.IoUtils._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends IOApp {

  val prepareEnv: IO[Unit] = IO {
    // todo does not work, deletes everything
//    Visualizer.Directory.toFile.listFiles().foreach(_.delete())
  }

  implicit val context: Context = Context(8, 50, c => c.head.toDouble, _ => x => x * x)

  val getInput: IO[String] = IO {
    println("Enter expression:")
    io.StdIn.readLine()
  }

  def printFailure(e: Throwable): IO[ExitCode] = IO {
    println("Invalid expression:")
    println(Show[Throwable].show(e))
  }.as(ExitCode.Error)

  def printSuccess[A](x: A): IO[ExitCode] = IO {
    println("Processed successfully")
  }.as(ExitCode.Success)

  def processResults(results: List[Writer[List[String], Either[String, ExecutionResult]]]): IO[Unit] = IO {
    val (correct, incorrect) = results.map(_.run).partition { case (_, res) => res.isRight }

    correct.map { case (log, res) => log -> res.right.get }
      .sortBy { case (_, res) => res.efficiency }
      .reverse
      .foreach { case (log, res) =>
        println()
        println("Processing CORRECTLY")
        println("Processing log:")
        log.foreach(println)
        println(asTreeShowable.show(res.expression))
        println(s"result = ${res.value}, execution time = ${res.time}, speedup = ${res.speedup}, efficiency = ${res.efficiency}")
      }

    incorrect.map { case (log, res) => log -> res.left.get }
      .foreach { case (log, err) =>
        println("Processing FAILED")
        println(s"Error message: $err")
        println("Processing log:")
        log.foreach(println)
      }
  }

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

  def runOnce(): IO[ExitCode] =
    expressionsIO
      .map(_.map(AlgebraicExpressionRunner.run))
      .flatMap(processResults)
      .redeemWith(printFailure, printSuccess)

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
