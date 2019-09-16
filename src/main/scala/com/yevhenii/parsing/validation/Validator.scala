package com.yevhenii.parsing.validation

import com.yevhenii.utils.Monoid
import com.yevhenii.utils.Instances._

import scala.annotation.tailrec

trait Validator[R] {
  type Err = List[String]

  def validate(x: R): Either[Err, R]
}

object Instances {
  val bracketsValidator = new Validator[String] {
    type Result = Either[Err, String]

    override def validate(exprStr: String): Either[Err, String] = {
      @tailrec def loop(depth: Int, index: Int, curr: Result): Result = (depth, index) match {
        case (0, ind) if ind == exprStr.length =>
          curr
        case (_, ind) if ind == exprStr.length =>
          addFailure(curr, err(ind))
        case (_, _) =>
          (depth, exprStr(index)) match {
            case (0, ')') => loop(0, index + 1, addFailure(curr, err(index)))
            case (d, ')') if d > 0 => loop(depth - 1, index + 1, curr)
            case (_, '(') => loop(depth + 1, index + 1, curr)
            case (_, _) => loop(depth, index + 1, curr)
          }
      }

      loop(0, 0, Right(exprStr))
    }

    def addFailure(prev: Result, err: Err): Result = {
      prev match {
        case Right(_) => Left(err)
        case Left(errors) => Left(Monoid.combine(errors, err))
      }
    }

    def err(ind: Int): Err = List(s"Error in brackets position/number, index = $ind")
  }
}