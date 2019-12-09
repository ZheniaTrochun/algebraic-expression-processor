package com.yevhenii.execution

import java.util.concurrent.{Callable, Executors, TimeUnit}

import com.yevhenii.execution.Execution.Flow

import scala.collection.mutable

trait Executor {

//  def submit(x: Flow[_]): Unit
  def submit(x: Callable[_], complexity: Int = 1): Unit
  def submitRunnable(x: Runnable, complexity: Int = 1): Unit

  def getDuration: Int
}

object Executor {
  class DataFlowExecutor(context: Context) extends Executor {
    val parallelism: Int = context.parallelism

    private[this] val tickTime: Int = context.tickTime
    private[this] var tacts = 0
    private[this] val workerThreadFactory = Executors.newScheduledThreadPool(1)

    private val queue: mutable.Queue[(Callable[_], Int)] = new mutable.Queue[(Callable[_], Int)]()
    private var currBuffer: mutable.Buffer[(Callable[_], Int)] = new mutable.ListBuffer[(Callable[_], Int)]()

    override def submit(x: Callable[_], complexity: Int): Unit = queue.enqueue(x -> complexity)
    override def submitRunnable(x: Runnable, complexity: Int): Unit = submit(() => x.run(), complexity)

    override def getDuration: Int = tacts

    private def tick(): Unit = {
      println(s"[${System.currentTimeMillis()}] tick")
      if (currBuffer.size < parallelism) {
        val diff = parallelism - currBuffer.size
        for (_ <- 1 to diff) {
          if (queue.nonEmpty) {
            currBuffer.append(queue.dequeue())
          }
        }
      }

      if (currBuffer.nonEmpty) {
        val newBuffer = new mutable.ListBuffer[(Callable[_], Int)]()

        for ((callable, complexity) <- currBuffer) {
          if (complexity == 1) {
            callable.call()
          } else {
            newBuffer.append(callable -> (complexity - 1))
          }
        }

        currBuffer = newBuffer
        tacts += 1
      }
    }

    def init(): Unit = workerThreadFactory.scheduleAtFixedRate(
      () => tick(),
      tickTime,
      tickTime,
      TimeUnit.MILLISECONDS
    )
  }
}
