package com.yevhenii.parsing.validation

import com.yevhenii.parsing.validation.Validator.Err
import com.yevhenii.utils.Monoid
import com.yevhenii.utils.Instances._

import scala.annotation.tailrec

trait Validator[R] {
  def validate(x: R): Either[Err, R]
}

object Validator {
  type Err = List[String]
  type Result = Either[Err, String]

  def addFailure(prev: Result, err: Err): Result = {
    prev match {
      case Right(_) => Left(err)
      case Left(errors) => Left(Monoid.combine(errors, err))
    }
  }

  val bracketsValidator: Validator[String] = (exprStr: String) => {
    def err(ind: Int): Err = List(s"Error in brackets position/number, index = $ind")

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


  val literalsValidatorCreator: Set[String] => Validator[String] = (literals: Set[String]) =>
    (exprStr: String) => {
      val parsedLiterals = exprStr.split("[^a-zA-Z]*").filterNot(_.isEmpty).toSet
      val illegalLiterals = parsedLiterals diff literals

      def err(literal: String): Err = List(s"Found unexpected literal, literal = $literal")
//      def err(literal: String, ind: Int): Err = List(s"Error in literals, literal = $literal, index = $ind")

//      def findLiteral(x: String): Int = {
//        lazy val first = s" $exprStr ".indexOf(s" $x ")
//        lazy val second = s" $exprStr ".indexOf(s" $x(")
//
//        if (first != -1) first - 1
//        else second - 1
//      }

      if (illegalLiterals.isEmpty) Right {
        exprStr
      } else Left {
        literals
          .toList
          .map(err)
          .fold[Err](Monoid.unit)(Monoid.combine[Err])
      }
    }
}