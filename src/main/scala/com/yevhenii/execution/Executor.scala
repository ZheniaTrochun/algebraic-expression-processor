package com.yevhenii.execution

trait Executor {

  val parallelism: Int

  def submit(x: Flow[_], complexity: Int): Unit
}
