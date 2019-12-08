package com.yevhenii.execution

import java.util.concurrent.{Callable, CountDownLatch}
import java.util.concurrent.atomic.AtomicReference

import cats.Monad

object AsyncFlowImpl {
  sealed trait Flow[+A] {
    def get()(implicit executor: Executor): A
  }

  case class UnitFlow[A](value: A) extends Flow[A] {
    override def get()(implicit executor: Executor): A = value
  }

  case class AsyncFlow[A](f: () => A) extends Flow[A] {
    def get()(implicit executor: Executor): A = {
      val countdown = new CountDownLatch(1)
      val result = new AtomicReference[A]()
      executor.submitRunnable(() => {
        result.set(f()) // todo f().get() ????
        countdown.countDown()
      })

      countdown.await()
      result.get()
    }
  }

  def mapAsync[A, B](fa: Flow[A])(f: A => B)(implicit executor: Executor): Flow[B] = AsyncFlow { () =>
    f(fa.get())
  }

  def map2Async[A, B, C](fa: Flow[A], fb: Flow[B])(f: (A, B) => C)(implicit executor: Executor): Flow[C] = AsyncFlow { () =>
    val countdown = new CountDownLatch(2)
    val resultA = new AtomicReference[A]()
    val resultB = new AtomicReference[B]()

    def acquire[T](ref: AtomicReference[T], instance: () => T) = {
      ref.set(instance())
      countdown.countDown()
    }

    def getVal[T](flow: Flow[T], ref: AtomicReference[T]) = flow match {
      case UnitFlow(value) =>
        acquire(ref, () => value)
      case AsyncFlow(valF) =>
        executor.submitRunnable(() => acquire(ref, valF))
    }

    getVal[A](fa, resultA)
    getVal[B](fb, resultB)

//
//    executor.submitRunnable(() => {
//      resultA.set(fa.get())
//      countdown.countDown()
//    })
//
//    executor.submitRunnable(() => {
//      resultB.set(fb.get())
//      countdown.countDown()
//    })

    countdown.await()
    f(resultA.get(), resultB.get())
  }

  def flatMap[A, B](fa: Flow[A])(f: A => Flow[B])(implicit executor: Executor): Flow[B] = {//AsyncFlow { () =>
    val countdown = new CountDownLatch(1)
    val result = new AtomicReference[A]()

    def acquire[T](ref: AtomicReference[T], instance: () => T) = {
      ref.set(instance())
      countdown.countDown()
    }

    def getVal[T](flow: Flow[T], ref: AtomicReference[T]) = flow match {
      case UnitFlow(value) =>
        acquire(ref, () => value)
      case AsyncFlow(valF) =>
        executor.submitRunnable(() => acquire(ref, valF))
    }

    getVal[A](fa, result)

    countdown.await()
    val a = result.get()

    f(a)
  }

  def pure[A](x: A): Flow[A] = UnitFlow(x)

//  implicit val flowMonad = new Monad[Flow] {
//    override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = AsyncFlowImpl.flatMap(fa)(f)
//
//    override def tailRecM[A, B](a: A)(f: A => Flow[Either[A, B]]): Flow[B] = ???
//
//    override def pure[A](x: A): Flow[A] = AsyncFlowImpl.pure(x)
//  }
}
