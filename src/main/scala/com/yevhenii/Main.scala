package com.yevhenii

import cats.{Monad, Show}
import cats.effect._
import cats.implicits._
import com.yevhenii.visualization.Visualizer._
import com.yevhenii.balancing.Balancer._
import com.yevhenii.optimisation.Optimiser._
import com.yevhenii.optimisation.Simplifier._
import com.yevhenii.parsing.FormulaParser
import com.yevhenii.parsing.FormulaParser.ParseError._
import com.yevhenii.utils.IoUtils._
import com.yevhenii.visualization.Visualizer

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends IOApp {

  val prepareEnv: IO[Unit] = IO {
    // todo does not work, deletes everything
//    Visualizer.Directory.toFile.listFiles().foreach(_.delete())
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

  val expressionsIO: IO[List[Expression]] = Monad[IO].compose[List].map {
    prepareEnv
      .flatMap(_ => getInput)
      .map(FormulaParser.apply)
      .flatMap(IO.fromEither)
      .map(optimize)
      .map(simplifyOneByOne)
  } (balance)
    .peek(visualizeResults)

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
