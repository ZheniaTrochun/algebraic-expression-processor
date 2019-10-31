package com.yevhenii.parsing

import cats.Show
import cats.effect.IO
import com.yevhenii.parsing.Expression._
import com.yevhenii.parsing.FormulaParser.ParseError._

object Main {

  val getInput: IO[String] = IO.apply {
    println("Enter expression:")
    io.StdIn.readLine()
  }

  def printFailure(e: Throwable): Unit = {
    println("Invalid expression:")
    println(Show[Throwable].show(e))
  }

  def printSuccess(x: Expression): Unit = {
    println("Parsed successfully, parsing tree:")
    println(Show[Expression].show(x))
  }

  val program: IO[Unit] = getInput
    .map(FormulaParser.apply)
    .flatMap(IO.fromEither)
    .redeem(printFailure, printSuccess)

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
