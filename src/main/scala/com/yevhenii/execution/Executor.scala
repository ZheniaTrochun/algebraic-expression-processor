package com.yevhenii.execution

import java.util.concurrent.{Callable, Executors, ScheduledFuture, TimeUnit}

import scala.collection.mutable

trait Executor {

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
    private[this] var tickSchedulled: Option[ScheduledFuture[_]] = None

    private val queue: mutable.Queue[(Callable[_], Int)] = new mutable.Queue[(Callable[_], Int)]()
    private var currBuffer: mutable.Buffer[(Callable[_], Int)] = new mutable.ListBuffer[(Callable[_], Int)]()

    override def submit(x: Callable[_], complexity: Int): Unit = queue.enqueue(x -> complexity)
    override def submitRunnable(x: Runnable, complexity: Int): Unit = submit(() => x.run(), complexity)

    override def getDuration: Int = tacts

    private def tick(): Unit = {
      println(s"[${System.currentTimeMillis()}] tick")
      if (currBuffer.size < parallelism) {
        val diff = parallelism - currBuffer.size
        println(s"enqueue max $diff events")
        for (_ <- 1 to diff) {
          if (queue.nonEmpty) {
            println("enqueue")
            currBuffer.append(queue.dequeue())
          }
        }
      }

      if (currBuffer.nonEmpty) {
        val newBuffer = new mutable.ListBuffer[(Callable[_], Int)]()

        for ((callable, complexity) <- currBuffer) {
          if (complexity == 1) {
            println("call")
            callable.call()
          } else {
            newBuffer.append(callable -> (complexity - 1))
          }
        }

        currBuffer = newBuffer
        tacts += 1
      }
    }

    def init(): Unit = {
      tickSchedulled = Some(workerThreadFactory.scheduleAtFixedRate(
        () => tick(),
        tickTime,
        tickTime,
        TimeUnit.MILLISECONDS
      ))
    }

    def stop(): Unit = tickSchedulled.foreach(_.cancel(true))
  }
}
