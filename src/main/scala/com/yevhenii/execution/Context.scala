package com.yevhenii.execution

case class Context(
  parallelism: Int,
  tickTime: Int,
  constants: String => Double,
  functions: String => (Double => Double)
)
