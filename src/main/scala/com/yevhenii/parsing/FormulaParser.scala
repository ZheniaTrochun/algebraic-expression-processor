package com.yevhenii.parsing

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object FormulaParser extends RegexParsers with PackratParsers {

  def id: Parser[Constant] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ Constant

  def number: Parser[Number] = "-" ~> number ^^ (n => Number(-n.value)) |
    ("[0-9]+\\.[0-9]*".r | "[0-9]+".r) ^^ (p => Number(p.toDouble))

  def funcCall: Parser[FuncCall] = id ~ ("(" ~> expression <~ ")") ^^ { case id ~ expr => FuncCall(id, expr) }

  def value: Parser[Expression] = number | funcCall | id | ("(" ~> expression <~ ")")

  lazy val term: PackratParser[Expression] = term ~ ("*" | "/") ~ value ^^ binOperation | value

  lazy val expression: PackratParser[Expression] = expression ~ ("+" | "-") ~ term ^^ binOperation | term

  def binOperation(p: Expression ~ String ~ Expression) = p match {
    case left ~ operator ~ right => BinOperation(left, BinOperator(operator), right)
  }

  def apply(expressionStr: String): Either[ParseError, Expression] = {
    parse(expression, new PackratReader(new CharSequenceReader(expressionStr))) match {
      case Success(result, next) => Right(result)
      case NoSuccess(err, next) => Left(ParseError(err))
    }
  }

  case class ParseError(msg: String)
}

sealed trait Expression

case class BinOperator(operator: String) extends Exception

case class Number(value: Double) extends Expression
case class Constant(name: String) extends Expression
case class BinOperation(left: Expression, operator: BinOperator, right: Expression) extends Expression
case class FuncCall(funcName: Constant, argument: Expression) extends Expression