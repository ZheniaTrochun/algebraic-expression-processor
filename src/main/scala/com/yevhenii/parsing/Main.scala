package com.yevhenii.parsing

import com.yevhenii.parsing.FormulaParser.ParseError

object Main extends App {

  println("Enter expression:")
  val input = io.StdIn.readLine()

  val res = FormulaParser.apply(input)

  res match {
    case Left(ParseError(msg, source)) =>
      println(msg)
      println(source)

    case Right(_) =>
      println("Everything is correct!")
  }
}
