package com.yevhenii.optimisation

import cats.{Id, Monad}
import com.yevhenii._
import com.yevhenii.ExpressionOps.expressionOps

import scala.annotation.tailrec

object Simplifier {

  def shouldBeSimplified(tree: Expression): Boolean = tree match {
    case _: Number                                                              => false
    case _: Constant                                                            => false
    case UnaryOperation(_: BinOperation, UnaryOperator('-'))                    => true
    case UnaryOperation(_: UnaryOperation, UnaryOperator('-'))                  => true
    case UnaryOperation(_: BracketedExpression, UnaryOperator('-'))             => true
    case UnaryOperation(_, _)                                                   => false
    case FuncCall(_, expr)                                                      => shouldBeSimplified(expr)
    case BinOperation(_: BracketedExpression, _, _)                             => true
    case BinOperation(_, BinOperator('*' | '+' | '-'), _: BracketedExpression)  => true
    case BinOperation(_, BinOperator('/'), _: BracketedExpression)              => false
    case BinOperation(left, _, right)                                           => shouldBeSimplified(left) || shouldBeSimplified(right)
    case BracketedExpression(inner)                                             => shouldBeSimplified(inner)
  }

  @tailrec
  def simplify(tree: Expression): Expression = {
    if (shouldBeSimplified(tree)) simplify {
      tree.traverse(simplifyMinuses)
        .traverse(simplifyMultiplication)
        .traverse(simplifyDivision)
        .traverse(removeUnnecessaryBrackets)
    } else {
      flattenBrackets(tree)
    }
  }

  private def simplifyMinuses(expression: Expression): Id[Expression] = Monad[Id].pure {
    expression match {
      case UnaryOperation(expr: BinOperation , UnaryOperator('-')) =>
        openUnaryMinus(expr)
      case UnaryOperation(expr: UnaryOperation , UnaryOperator('-')) =>
        openUnaryMinus(expr)
      case UnaryOperation(expr: BracketedExpression , UnaryOperator('-')) =>
        openUnaryMinus(expr)

      case x => x
    }
  }

  private def simplifyMultiplication(expression: Expression): Id[Expression] = {
    expression match {
      case BinOperation(BracketedExpression(left), BinOperator('*'), right) =>
        BracketedExpression(openMultiplication(left, right))
      case BinOperation(left, BinOperator('*'), BracketedExpression(right)) =>
        BracketedExpression(openMultiplication(right, left))

      case x => x
    }
  }

  private def simplifyDivision(expression: Expression): Id[Expression] = {
    expression match {
      case BinOperation(BracketedExpression(left), BinOperator('/'), right) =>
        BracketedExpression(openDivision(left, right))

      case x => x
    }
  }

  private def removeUnnecessaryBrackets(expression: Expression): Id[Expression] = {
    expression match {

      case BinOperation(BracketedExpression(left), BinOperator('+'), right) => BinOperation(left, BinOperator('+'), right)
      case BinOperation(left, BinOperator('+'), BracketedExpression(right)) => BinOperation(left, BinOperator('+'), right)

      case BinOperation(BracketedExpression(left), BinOperator('-'), right) => BinOperation(left, BinOperator('-'), right)
      case BinOperation(left, BinOperator('-'), BracketedExpression(right)) => BinOperation(left, BinOperator('-'), right)

      case x => x
    }
  }

  def flattenBrackets(expression: Expression): Expression = expression match {
    case BracketedExpression(inner) => flattenBrackets(inner)
    case BinOperation(left, BinOperator('/'), BracketedExpression(right)) => BinOperation(flattenBrackets(left), BinOperator('/'), BracketedExpression(flattenBrackets(right)))
    case BinOperation(left, op, right) => BinOperation(flattenBrackets(left), op, flattenBrackets(right))
    case UnaryOperation(inner, op) => UnaryOperation(flattenBrackets(inner), op)
    case FuncCall(name, inner) => FuncCall(name, flattenBrackets(inner))
    case x => x
  }

  def openUnaryMinus(expression: Expression): Expression = expression match {
    case UnaryOperation(inner, UnaryOperator('-')) =>
      inner
    case Number(value) =>
      Number(-value)
    case c: Constant =>
      UnaryOperation(c, UnaryOperator('-'))
    case BinOperation(left, BinOperator('*'), right) =>
      BinOperation(openUnaryMinus(left), BinOperator('*'), right)
    case BinOperation(left, BinOperator('+'), right) =>
      BinOperation(openUnaryMinus(left), BinOperator('+'), openUnaryMinus(right))
    case other =>
      UnaryOperation(other, UnaryOperator('-'))
  }

  def openMultiplication(insideBrackets: Expression, expr: Expression): Expression = insideBrackets match {
    case n: Number =>
      BinOperation(n, BinOperator('*'), expr)
    case c: Constant =>
      BinOperation(c, BinOperator('*'), expr)
    case f: FuncCall =>
      BinOperation(f, BinOperator('*'), expr)
    case b: BracketedExpression =>
      BinOperation(b, BinOperator('*'), expr)
    case UnaryOperation(inner, op) =>
      UnaryOperation(openMultiplication(inner, expr), op)
    case BinOperation(left, BinOperator('+'), right) =>
      BinOperation(openMultiplication(left, expr), BinOperator('+'), openMultiplication(right, expr))
    case BinOperation(left, op @ BinOperator('*' | '/'), right) =>
      BinOperation(openMultiplication(left, expr), op, right)
  }

  def openDivision(insideBrackets: Expression, expr: Expression): Expression = insideBrackets match {
    case n: Number =>
      BinOperation(n, BinOperator('/'), BracketedExpression(expr))
    case c: Constant =>
      BinOperation(c, BinOperator('/'), BracketedExpression(expr))
    case f: FuncCall =>
      BinOperation(f, BinOperator('/'), BracketedExpression(expr))
    case b: BracketedExpression =>
      BinOperation(b, BinOperator('/'), BracketedExpression(expr))
    case UnaryOperation(inner, op) =>
      UnaryOperation(openDivision(inner, BracketedExpression(expr)), op)
    case BinOperation(left, BinOperator('+'), right) =>
      BinOperation(openDivision(left, expr), BinOperator('+'), openDivision(right, expr))
    case BinOperation(left, op @ BinOperator('*' | '/'), right) =>
      BinOperation(openDivision(left, expr), op, BracketedExpression(right))
  }
}
