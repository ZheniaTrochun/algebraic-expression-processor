package com.yevhenii.execution

import java.util.concurrent.{Callable, Executors}

import scala.collection.mutable

trait Executor {

  val parallelism: Int

  def submit(x: Callable[_], complexity: Int = 1): Unit
  def submitRunnable(x: Runnable, complexity: Int = 1): Unit

  def getDuration: Int
}

object Executor {
  object DataFlowExecutor extends Executor {
    override val parallelism: Int = 2

    private[this] var tacts = 0

    private val queue: mutable.Queue[Callable[_]] = new mutable.Queue[Callable[_]]()

    private var currBuffer: mutable.Buffer[(Callable[_], Int)] = new mutable.ListBuffer[(Callable[_], Int)]()

    private[this] val workerThreadFactory = Executors.newFixedThreadPool(1)//.submit(new Runnable {
//      override def run(): Unit = while (true) tick()
//    })

//    override def submit(x: Flow[_], complexity: Int): Unit = ???

    override def submit(x: Callable[_], complexity: Int): Unit = queue.enqueue(x)
    override def submitRunnable(x: Runnable, complexity: Int): Unit = queue.enqueue(() => x.run())

    override def getDuration: Int = {
      val t = tacts
      tacts = 0
      t
    }

    private def tick(): Unit = {
      if (currBuffer.size < parallelism) {
        val diff = parallelism - currBuffer.size
        for (_ <- 1 to diff) {
          if (queue.nonEmpty) {
            currBuffer.append(queue.dequeue() -> 1)
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
      Thread.sleep(500)
    }

    def init(): Unit = {
      workerThreadFactory.submit(new Runnable {
        override def run(): Unit = while (true) tick()
      })
    }
  }
}
