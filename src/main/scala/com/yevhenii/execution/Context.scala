package com.yevhenii.execution

case class Context(
  parallelism: Int,
  constants: Map[String, Double],
  functions: Map[String, Double => Double]
)
