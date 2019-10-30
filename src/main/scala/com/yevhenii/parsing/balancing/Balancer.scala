package com.yevhenii.parsing.balancing

import com.yevhenii.parsing._

object Balancer {

  def balance(exprTree: Expression): Expression = {
    exprTree match {
      case x: Number => x
      case x: Constant => x
      case x: BinOperation => loop(x)
      case FuncCall(name, x) => FuncCall(name, loop(x))
    }
  }

  def loop(curr: Expression, stack: List[Expression] = Nil): Expression = ???
}
