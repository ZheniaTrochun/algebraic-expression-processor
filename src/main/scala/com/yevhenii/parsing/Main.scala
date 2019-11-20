package com.yevhenii.parsing

import cats.Show
import cats.effect.IO
import com.yevhenii.parsing.Expression.asExpressionShowable
import com.yevhenii.parsing.balancing.Balancer._
import com.yevhenii.parsing.optimisation.Optimiser._
import com.yevhenii.parsing.optimisation.Simplifier._
import com.yevhenii.parsing.FormulaParser.ParseError._
import com.yevhenii.parsing.utils.IoUtils._
import reftree.render.{Renderer, RenderingOptions}
import reftree.diagram.Diagram
import java.nio.file.Paths

object Main {

  val renderer = Renderer(
    renderingOptions = RenderingOptions(density = 75),
    directory = Paths.get("./visualization"),
    format = "pdf"
  )

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
//    println(s"Result expression: ${Show[Expression].show(x)}")
    println("Parsing tree: ./visualization/balanced.pdf")
  }

  def visualize(stage: String)(expression: Expression): IO[Unit] = IO.apply {
    import renderer._
    Diagram.sourceCodeCaption(expression)
      .withCaption(Show[Expression].show(expression))
      .render(stage)
  }

  val program: IO[Unit] = getInput
    .map(FormulaParser.apply)
    .flatMap(IO.fromEither)
    .map(x => optimize(x))
    .peek(visualize("optimized"))
    .map(x => simplify(x))
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
