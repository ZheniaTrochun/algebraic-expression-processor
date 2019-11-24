package com.yevhenii.visualization

import java.nio.file.{Path, Paths}

import cats.effect.IO
import cats.{Id, Monad}
import com.yevhenii._
import com.yevhenii.ExpressionOps._
import reftree.diagram.Diagram
import reftree.render.{Renderer, RenderingOptions}

object Visualizer {
  val Directory: Path = Paths.get("./visualization")
  val FinalOutputFile: String = getOutputFile("balanced")

  def getOutputFile(stage: String): String = s"${Directory.toString}/$stage.pdf"

  private val renderer = Renderer(
    renderingOptions = RenderingOptions(density = 75),
    directory = Directory,
    format = "pdf"
  )

  def visualize(stage: String)(expression: Expression): IO[Unit] = IO.apply {
    import renderer._
    Diagram.sourceCodeCaption(expression.traverse(prettifyOutput))
      .withCaption(asExpressionShowable.show(expression))
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
