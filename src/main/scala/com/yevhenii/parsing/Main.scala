package com.yevhenii.parsing

import cats.Show
import cats.effect.IO
import com.yevhenii.parsing.Visualizer._
import com.yevhenii.parsing.balancing.Balancer._
import com.yevhenii.parsing.optimisation.Optimiser._
import com.yevhenii.parsing.optimisation.Simplifier._
import com.yevhenii.parsing.FormulaParser.ParseError._
import com.yevhenii.parsing.utils.IoUtils._

object Main {

  val getInput: IO[String] = IO.apply {
    println("Enter expression:")
    io.StdIn.readLine()
  }

  def printFailure(e: Throwable): IO[Unit] = IO.apply {
    println("Invalid expression:")
    println(Show[Throwable].show(e))
  }

  def printSuccess(x: Expression): IO[Unit] = IO.apply {
    println("Parsed successfully")
    println(s"Parsing tree: ${Visualizer.FinalOutputFile}")
  }

  val program: IO[Unit] = getInput
    .map(FormulaParser.apply)
    .flatMap(IO.fromEither)
    .map(optimize)
    .peek(visualize("optimized"))
    .map(simplify)
    .peek(visualize("opened_parenthesis"))
    .map(x => balance(x))
    .peek(visualize("balanced"))
    .redeemWith(printFailure, printSuccess)

  def runOnce(): Unit = {
    program.unsafeRunSync()
  }

  def runAsRepl(): Unit = {
    while (true) {
      runOnce()
    }
  }

  def run(args: Array[String]): Unit = {
    if (args.contains("repl")) runAsRepl()
    else runOnce()
  }

  def main(args: Array[String]): Unit = run(args)
}
