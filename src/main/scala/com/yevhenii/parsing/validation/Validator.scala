package com.yevhenii.parsing.validation

import com.yevhenii.parsing.validation.Validator.Err
import cats.Monoid
import cats.implicits._
import com.yevhenii.parsing.FormulaParser

import scala.collection.parallel.ParSeq
import scala.annotation.tailrec

trait Validator[R] {
  def validate(x: R): Either[Err, R]
}

object Validator {
  type Err = List[String]
  type Result = Either[Err, String]

  val allowedOperators = Set('+', '-', '*', '/')

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
      val parsedLiterals = exprStr.split("[^a-zA-Z]{1,}").filterNot(_.isEmpty).toSet
      val illegalLiterals = parsedLiterals diff literals

      def err(literal: String): Err = List(s"Found unexpected literal, literal = $literal")

      if (illegalLiterals.isEmpty) Right {
        exprStr
      } else Left {
        illegalLiterals
          .toList
          .map(err)
          .fold(Monoid[Err].empty)(Monoid[Err].combine)
      }
    }

  val operatorValidator: Validator[String] = { exprStr =>
    val notOperatorRegex = "[a-zA-Z0-9_\\.\\(\\)\\s]*"
    val operators = exprStr.replaceAll(notOperatorRegex, "").toSet
    val notExpected = operators diff allowedOperators

    if (notExpected.isEmpty) {
      Right(exprStr)
    } else {
      Left(notExpected.toList.map(op => s"Unexpected operator: $op"))
    }
  }

  val regexValidator: Validator[String] = { exprStr =>
    val isOperatorAtTheEnd = "[-+*/]$".r
    val isOperatorBeforeBracket = "[-+*/]\\)".r
    val isOperatorAfterBracket = "\\([*/+]".r

    val checklist = isOperatorAtTheEnd :: isOperatorBeforeBracket :: isOperatorAfterBracket :: Nil

    checklist.foldLeft[Result](Right(exprStr)) { (result, regex) =>
      regex.findAllIn(exprStr).toList match {
        case Nil => result
        case nonEmpty => addFailure(result, nonEmpty.map(s => s"unexpected token: $s"))
      }
    }
  }

  val tryParse: Validator[String] = { exprStr =>
    FormulaParser(exprStr).map(_ => exprStr).left.map(e => List(e.msg))
  }

  def validate(literals: Set[String])(expression: String): Result = {
    val validationSeq = ParSeq(
      bracketsValidator,
      literalsValidatorCreator(literals),
      operatorValidator,
      regexValidator,
      tryParse
    )

//    validationSeq.map(v => v.validate(expression)).fold(Right(expression)) { (first, second) =>
//      (first, second) match {
//        case (Right(_), Right(_)) => first
//        case (Left(err1), Left(err2)) => Left(err1 ::: err2)
//        case (Left(err), Right(_)) => first
//        case (Right(_), Left(err)) => second
//      }
//    }

//    validationSeq.map(v => v.validate(expression)).fold[Result](Monoid.empty)(Monoid.combine(_, _))
    Monoid[Result].empty
  }

  implicit def resultMonoid: Monoid[Result] = new Monoid[Result] {
    override def empty: Result = Right("")

    override def combine(x: Result, y: Result): Result = (x, y) match {
      case (Right(_), Right(_)) => x
      case (Left(e1), Left(e2)) => Left(Monoid[Err].combine(e1, e2))
      case (Left(_), _) => x
      case (_, Left(_)) => y
    }
  }
}