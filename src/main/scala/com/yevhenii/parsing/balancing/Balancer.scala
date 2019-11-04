package com.yevhenii.parsing.balancing

import com.yevhenii.parsing._

import math.max
import math.abs

object Balancer {

  def balance(exprTree: Expression, guard: Int = 0): Expression = {
    if (guard == 1000) exprTree
    else {
      exprTree match {
        case x @ BinOperation(left, op, right) if shouldBalance(x) =>
  //        balanceBinary(BinOperation(balance(left), op, balance(right)))
          balance(balanceBinary(BinOperation(balance(left, guard + 1), op, balance(right, guard + 1))), guard + 1)
        case FuncCall(name, inner) =>
          FuncCall(name, balance(inner, guard + 1))
        case BracketedExpression(inner) =>
          BracketedExpression(balance(inner, guard + 1))
        case UnaryOperation(inner, op) =>
          UnaryOperation(balance(inner, guard + 1), op)
        case BinOperation(left, op, right) =>
          BinOperation(balance(left, guard + 1), op, balance(right, guard + 1))
        case x => x
      }
    }
  }

  def shouldBalance(exprTree: Expression): Boolean = {
    exprTree match {
      case BinOperation(_, BinOperator("/"), _) => false
      case BinOperation(left, _, right) => abs(size(left) - size(right)) > 1
      case _ => false
    }
  }

  def balanceBinary(root: BinOperation): Expression = root match {
    case BinOperation(BinOperation(left, leftOp, leftRight), op, rightRoot) if shouldRotateLeft(root) =>
      BinOperation(left, leftOp, BinOperation(leftRight, op, rightRoot))

    case BinOperation(leftRoot, op, BinOperation(rightLeft, rightOp, right)) if shouldRotateRight(root) =>
      BinOperation(BinOperation(leftRoot, op, rightLeft), rightOp, right)

    case _ => root
  }

  def shouldRotateLeft(tree: BinOperation): Boolean = tree match {
    case BinOperation(l @ BinOperation(_, lop, _), op, r) if size(l) - size(r) > 1 =>
      checkOperations(lop, op)
    case _ => false
  }

  def shouldRotateRight(tree: BinOperation): Boolean = tree match {
    case BinOperation(l, op, r @ BinOperation(_, rop, _)) if size(r) - size(l) > 1 =>
      checkOperations(op, rop)
    case _ => false
  }

  def checkOperations(lop: BinOperator, rop: BinOperator): Boolean = (lop, rop) match {
    case (BinOperator("+"), BinOperator("+")) => true
    case (BinOperator("-"), BinOperator("-")) => true
    case (BinOperator("*"), BinOperator("*")) => true
    case (BinOperator("-"), BinOperator("+")) => true
    case (BinOperator("+"), BinOperator("-")) => true
    case _ => false
  }

  def size(exprTree: Expression): Int = {
    exprTree match {
      case Number(_) => 1
      case Constant(_) => 1
      case BinOperation(left, _, right) => 1 + max(size(left), size(right))
      case FuncCall(_, x) => 1 + size(x)
      case BracketedExpression(x) => size(x)
      case UnaryOperation(x, _) => size(x)
    }
  }
}
