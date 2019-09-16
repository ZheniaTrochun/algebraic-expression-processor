package com.yevhenii.parsing.validation

import com.yevhenii.utils.Monoid
import com.yevhenii.utils.Instances._

import scala.annotation.tailrec

trait Validator[L, R] {
  def validate(x: R): Either[L, R]
}

object Validator {
  val bracketsValidator = new Validator[List[String], String] {
    type Err = List[String]
    type Result = Either[Err, String]

    override def validate(exprStr: String): Either[List[String], String] = {
      @tailrec def loop(depth: Int, index: Int, curr: Result): Either[List[String], String] = (depth, index) match {
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