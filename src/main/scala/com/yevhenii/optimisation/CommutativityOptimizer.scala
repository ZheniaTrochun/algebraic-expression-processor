package com.yevhenii.optimisation

import cats.{Id, Monad}
import com.yevhenii._
import com.yevhenii.ExpressionOps._

object CommutativityOptimizer {

  def orderByComplexity(expression: Expression): Expression = {
    expression.traverse(sortByComplexity)
  }

  def permutations(expression: Expression): List[Expression] = expression match {
    case BinOperation(left, op, right) =>
      val leftPermutations = permutations(left)
      val rightPermutations = permutations(right)

      val pairs = leftPermutations cross rightPermutations

      pairs.flatMap { case (l, r) => permutation(BinOperation(l, op, r)) }

    case FuncCall(name, inner) => permutations(inner).map(x => FuncCall(name, x))
    case BracketedExpression(inner) => permutations(inner).map(BracketedExpression.apply)
    case UnaryOperation(inner, op) => permutations(inner).map(x => UnaryOperation(x, op))
    case x => x :: Nil
  }

  private def permutation(expression: Expression): List[Expression] = expression match {
    case original @ BinOperation(left, BinOperator('+' | '*'), right)
      if evaluate(left) == evaluate(right) && !isSimilar(left, right) =>
      original :: swap(original) :: Nil
    case original =>
      original :: Nil
  }

  private def sortByComplexity(expression: Expression): Id[Expression] = Monad[Id].pure {
    expression match {
      case original @ BinOperation(left, op @ BinOperator('+' | '*'), right) =>
        val leftWeight = evaluate(left)
        val rightWeight = evaluate(right)
        lazy val isRightNegative = isNegative(right)

        if (rightWeight > leftWeight || isRightNegative) swap(original)
        else original

      case x => x
    }
  }

  private def evaluate(expression: Expression): Int = expression match {
    case BinOperation(left, BinOperator('+'), right) => 1 + evaluate(left) + evaluate(right)
    case BinOperation(left, BinOperator('_'), right) => 1 + evaluate(left) + evaluate(right)
    case BinOperation(left, BinOperator('*'), right) => 2 + evaluate(left) + evaluate(right)
    case BinOperation(left, BinOperator('/'), right) => 4 + evaluate(left) + evaluate(right)
    case FuncCall(_, inner) => 5 + evaluate(inner)
    case BracketedExpression(inner) => evaluate(inner)
    case UnaryOperation(inner, _) => evaluate(inner)
    case _ => 0
  }

  private def isNegative(expression: Expression): Boolean = expression match {
    case UnaryOperation(_, UnaryOperator('-')) => true
    case Number(value) if value < 0 => true
    case BinOperation(left, BinOperator('*' | '/'), right) => isNegative(left) || isNegative(right)
    case BracketedExpression(expr) => isNegative(expr)
    case _ => false
  }

  private def isSimilar(first: Expression, second: Expression): Boolean = (first, second) match {
    case (BinOperation(firstLeft, firstOp, firstRight), BinOperation(secondLeft, secondOp, secondRight)) if firstOp == secondOp =>
      isSimilar(firstLeft, secondLeft) && isSimilar(firstRight, secondRight)
    case (UnaryOperation(left, leftOp), UnaryOperation(right, rightOp)) if leftOp == rightOp =>
      isSimilar(left, right)
    case (BracketedExpression(left), BracketedExpression(right)) =>
      isSimilar(left, right)
    case (FuncCall(leftName, left), FuncCall(rightName, right)) if leftName == rightName =>
      isSimilar(left, right)

    case (_: ExpressionTreeLeaf, _: ExpressionTreeLeaf) => true
    case _ => false
  }

  private def swap(expression: Expression): Expression = expression match {
    case BinOperation(left, op, right) => BinOperation(right, op, left)
    case x => x
  }

  implicit class Crossable[X](xs: List[X]) {
    def cross[Y](ys: List[Y]): List[(X, Y)] = for { x <- xs; y <- ys } yield (x, y)
  }
}
