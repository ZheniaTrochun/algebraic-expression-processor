package com.yevhenii.execution

import java.io.FileWriter
import java.util.concurrent.{Callable, Executors, ScheduledFuture, TimeUnit}

import cats.data.Writer
import com.typesafe.scalalogging.LazyLogging
import com.yevhenii.execution.Execution.Flow

import scala.collection.mutable
import scala.io.Source

trait Executor {
  type Time = Int
  type Log = String
  type Err = String

  def submit(x: Callable[_], complexity: Int = 1): Unit
  def submitRunnable(x: Runnable, complexity: Int = 1): Unit

  def run[A](flow: Flow[A]): Writer[Log, Either[Err, (A, Time)]]
}

object Executor {
  class DataFlowExecutor(context: Context) extends Executor with LazyLogging {
    val parallelism: Int = context.parallelism
    private val file = "logs/log.log" // todo ugly

    private[this] val tickTime: Int = context.tickTime
    private[this] var tacts = 0
    private[this] val workerThreadFactory = Executors.newScheduledThreadPool(1)
    private[this] var tickSchedulled: Option[ScheduledFuture[_]] = None

    private val queue: mutable.Queue[(Callable[_], Int)] = new mutable.Queue[(Callable[_], Int)]()
    private var currBuffer: mutable.Buffer[(Callable[_], Int)] = new mutable.ListBuffer[(Callable[_], Int)]()

    override def submit(x: Callable[_], complexity: Int): Unit = queue.enqueue(x -> complexity)
    override def submitRunnable(x: Runnable, complexity: Int): Unit = submit(() => x.run(), complexity)

    private def getDuration: Int = tacts

    private def tick(): Unit = {
      logger.info(s"tick start [${tacts}]")
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

    private def getCapturedLog(): String = {
      val logSource = Source.fromFile(file)
      val log = logSource.mkString
      logSource.close()
      val fw = new FileWriter(file ,false)
      fw.write(" ")
      log
    }

    private def init(): Unit = {
      logger.info("Init executor service")
      tickSchedulled = Some(workerThreadFactory.scheduleAtFixedRate(
        () => tick(),
        tickTime,
        tickTime,
        TimeUnit.MILLISECONDS
      ))
    }

    private def stop(): Unit = {
      logger.info("Stop executor service")
      tickSchedulled.foreach(_.cancel(true))
    }

    override def run[A](flow: Flow[A]): Writer[Log, Either[Err, (A, Time)]] = {
      init()
      val flowRes = Flow.run(this)(flow)
      stop()
      Writer(getCapturedLog(), flowRes.map(_ -> getDuration))
    }
  }
}
