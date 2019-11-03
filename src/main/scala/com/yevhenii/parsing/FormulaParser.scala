package com.yevhenii.parsing

import cats.Show

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object FormulaParser extends RegexParsers with PackratParsers {

  def id: Parser[Constant] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ Constant

  def number: Parser[Number] = "-" ~> number ^^ (n => Number(-n.value)) |
    ("[0-9]+\\.[0-9]*".r | "[0-9]+".r) ^^ (p => Number(p.toDouble))

  def funcCall: Parser[FuncCall] = id ~ ("(" ~> expression <~ ")") ^^ { case id ~ expr => FuncCall(id, expr) }

  def value: Parser[Expression] = number | funcCall | id | bracketed

  lazy val bracketed: PackratParser[Expression] = ("(" ~> expression <~ ")") ^^ (x => BracketedExpression(x))

  lazy val term: PackratParser[Expression] = term ~ ("*" | "/") ~ value ^^ binOperation | value

  lazy val expression: PackratParser[Expression] = expression ~ ("+" | "-") ~ term ^^ binOperation | term

  def binOperation(p: Expression ~ String ~ Expression): BinOperation = p match {
    case left ~ operator ~ right => BinOperation(left, BinOperator(operator), right)
  }

  def apply(expressionStr: String): Either[ParseError, Expression] = {
    parseAll(expression, new PackratReader(new CharSequenceReader(expressionStr))) match {
      case Success(result, _) =>
        Right(result)
      case NoSuccess(err, next) =>
        Left(ParseError(err, next.pos.longString))
    }
  }

  case class ParseError(msg: String, source: String) extends Exception {
    override def getMessage: String = s"$msg\n$source"
  }

  object ParseError {
    implicit val errorShow: Show[Throwable] = new Show[Throwable] {
      override def show(t: Throwable): String = t.getMessage
    }
  }
}
