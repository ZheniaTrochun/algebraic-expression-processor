package com.yevhenii.parsing

import com.yevhenii.parsing.ExpressionOps.expressionOps
import com.yevhenii.parsing.ExpressionOps.asExpressionShowable
import reftree.render.{Renderer, RenderingOptions}
import reftree.diagram.Diagram
import java.nio.file.{Path, Paths}

import cats.{Id, Monad, Show}
import cats.effect.IO

object Visualizer {
  val directory: Path = Paths.get("./visualization")
  val FinalOutputFile: String = getOutputFile("balanced")

  def getOutputFile(stage: String): String = s"${directory.toString}/$stage.pdf"

  private val renderer = Renderer(
    renderingOptions = RenderingOptions(density = 75),
    directory = directory,
    format = "pdf"
  )

  def visualize(stage: String)(expression: Expression): IO[Unit] = IO.apply {
    import renderer._
    Diagram.sourceCodeCaption(expression.traverse(prettifyOutput))
      .withCaption(Show[Expression].show(expression))
      .render(stage)
  }

  private def prettifyOutput(expression: Expression): Id[Expression] = Monad[Id].pure {
    expression match {
      case Number(num) => Number(num)
      case Constant(name) => Constant(new String(name.getBytes))
      case BinOperation(left, BinOperator(op), right) => BinOperation(prettifyOutput(left), BinOperator(op), prettifyOutput(right))
      case UnaryOperation(inner, UnaryOperator(op)) => UnaryOperation(prettifyOutput(inner), UnaryOperator(op))
      case FuncCall(Constant(name), inner) => FuncCall(Constant(new String(name.getBytes)), prettifyOutput(inner))
      case BracketedExpression(inner) => BracketedExpression(prettifyOutput(inner))
    }
  }
}
