package com.yevhenii.parsing

import cats.{Monad, Show}

import scala.language.higherKinds

object ExpressionOps {

  implicit val asTreeShowable: Show[Expression] = new Show[Expression] {
    def showLoop(x: Expression, n: Int): String = x match {
      case Number(value) =>
        tabs(n) + s"$value\n"
      case Constant(name) =>
        tabs(n) + s"$name\n"
      case BinOperation(left, BinOperator(op), right) =>
        showLoop(left, n + 1) + tabs(n) +  op + "\n" + showLoop(right, n + 1)
      case FuncCall(Constant(name), arg) =>
        tabs(n) + s"$name(\n${showLoop(arg, n + 1)}" + tabs(n) + ")\n"
      case BracketedExpression(expr) =>
        tabs(n) + s"(\n${showLoop(expr, n + 1)}" + tabs(n) + ")\n"
      case UnaryOperation(expr, UnaryOperator(op)) =>
        tabs(n) + s"$op(\n${showLoop(expr, n + 1)}" + tabs(n) + ")\n"
    }

    def tabs(n: Int): String = "\t" * n

    override def show(x: Expression): String = showLoop(x, 0)
  }

  implicit val asExpressionShowable: Show[Expression] = new Show[Expression] {
    override def show(x: Expression): String = x match {
      case Number(value) =>
        s"$value"
      case Constant(name) =>
        s"$name"
      case BinOperation(left, BinOperator(op), right) =>
        show(left) + op + show(right)
      case FuncCall(Constant(name), arg) =>
        s"$name(${show(arg)}"
      case BracketedExpression(expr) =>
        s"(${show(expr)})"
      case UnaryOperation(expr, UnaryOperator(op)) =>
        s"$op(${show(expr)})"
    }
  }

  implicit val expressionTraversableTree = new TraversableTree[Expression] {
    override def traverse[F[_] : Monad](tree: Expression)(f: Expression => F[Expression]): F[Expression] = tree match {
      case number: Number =>
        f(number)
      case constant: Constant =>
        f(constant)
      case BinOperation(left, op, right) =>
        monadFlatMap2(traverse(left)(f), traverse(right)(f))(BinOperation(_, op, _))(f)
      case FuncCall(name, expr) =>
        monadFlatMap(traverse(expr)(f))(FuncCall(name, _))(f)
      case BracketedExpression(expr) =>
        monadFlatMap(traverse(expr)(f))(BracketedExpression.apply)(f)
      case UnaryOperation(expr, op) =>
        monadFlatMap(traverse(expr)(f))(UnaryOperation(_, op))(f)
    }

    private def monadFlatMap[F[_]: Monad](
      fa: F[Expression])(
      g: Expression => Expression)(
      f: Expression => F[Expression]
    ): F[Expression] = {

      Monad[F].flatMap(Monad[F].map(fa)(g))(f)
    }

    private def monadFlatMap2[F[_]: Monad](
      fl: F[Expression], fr: F[Expression])(
      g: (Expression, Expression) => Expression)(
      f: Expression => F[Expression]
    ): F[Expression] = {

      Monad[F].flatMap(Monad[F].map2(fl, fr)(g))(f)
    }
  }

  implicit class expressionOps(expression: Expression) {
    def show(): String = asTreeShowable.show(expression)

    def traverse[F[_]: Monad](f: Expression => F[Expression]): F[Expression] =
      TraversableTree[Expression].traverse(expression)(f)
  }
}
