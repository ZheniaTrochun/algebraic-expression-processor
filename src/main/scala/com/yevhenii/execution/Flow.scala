package com.yevhenii.execution

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import annotation.tailrec

object Execution {

  trait Future[+A] {
    def apply(k: A => Unit): Unit
  }

  trait Flow[+A] extends (Executor => Future[A])

  case class AsyncFlow[+A](f: (Executor, Int) => Future[A], complexity: Int) extends Flow[A] {
    override def apply(v1: Executor): Future[A] = f(v1, complexity)
  }

  case class UnitFlow[+A](value: A) extends Flow[A] {
    override def apply(v1: Executor): Future[A] = new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(value)
    }
  }

  object Flow {

    // todo return Either or Writer[Either]]
    def run[A](es: Executor)(p: Flow[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a =>
        ref.set(a)
        latch.countDown()
      }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Flow[A] = UnitFlow(a)

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Flow[A] = AsyncFlow (
      (es: Executor, _) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }, 1
    )

    def delay[A](a: => A, complexity: Int): Flow[A] = AsyncFlow (
      (es: Executor, _) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      },
      complexity
    )

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Flow[A] = AsyncFlow (
      (es: Executor, _) => new Future[A] {
        def apply(k: A => Unit) = f(k)
      }, 1
    )

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: Executor)(r: => Unit, complexity: Int): Unit =
      es.submit(new Callable[Unit] { def call = r }, complexity)

    // todo 1+2+3+4+5 run on 2 cores in 2 tacts which is clearly incorrect
    def map2withComplexity[A,B,C](p: Flow[A], p2: Flow[B])(f: (A,B) => C, fComplexity: Int): Flow[C] = AsyncFlow (
      (es: Executor, complexity) => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          (p, p2) match {
            case (UnitFlow(v1), UnitFlow(v2)) => println(s"map2 unit($v1), unit($v2)")
            case (UnitFlow(v1), AsyncFlow(f, c)) => println(s"map2 unit($v1), async(func, $c)")
            case (AsyncFlow(f, c), UnitFlow(v2)) => println(s"map2 async(func, $c), unit($v2)")
            case (AsyncFlow(f1, c1), AsyncFlow(f2, c2)) => println(s"map2 async(func, $c1), async(func, $c2)")
          }

          val countDown = new CountDownLatch(2)
          val ar = new AtomicReference[A]()
          val br = new AtomicReference[B]()

          p match {
            case UnitFlow(x) =>
              ar.set(x)
              countDown.countDown()
            case AsyncFlow(ap, c) => p(es) { a =>
              ar.set(a)
              countDown.countDown()
            }
          }

          p2 match {
            case UnitFlow(x) =>
              br.set(x)
              countDown.countDown()
            case AsyncFlow(ap, c) => p2(es) { b =>
              br.set(b)
              countDown.countDown()
            }
          }

          // todo check
//          countDown.await()
//          val a = ar.get
//          val b = br.get

          eval(es)({
            countDown.await()
            cb(f(ar.get(), br.get()))
          }, complexity)
        }
      }, fComplexity
    )

    // specialized version of `map`
    def mapAsync[A,B](p: Flow[A])(f: A => B, complexity: Int): Flow[B] = AsyncFlow (
      (es: Executor, complexity) => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es)(cb(f(a)), complexity))
      }, complexity
    )

    def map[A,B](p: Flow[A])(f: A => B): Flow[B] = new Flow[B] {
      override def apply(es: Executor): Future[B] = new Future[B] {
        override def apply(cb: B => Unit): Unit = {
          p(es)(a => cb(f(a)))
        }
      }
    }

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] =
      flatMap(p)(f)

    def flatMap[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] = AsyncFlow (
      (es: Executor, _) => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      },
      1
    )

    implicit def toParOps[A](p: Flow[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Flow[A]) {
      def map[B](f: A => B): Flow[B] = Flow.map(p)(f)
      def map2[B,C](b: Flow[B])(f: (A,B) => C, fComplexity: Int): Flow[C] = Flow.map2withComplexity(p,b)(f, fComplexity)
      def flatMap[B](f: A => Flow[B]): Flow[B] = Flow.flatMap(p)(f)
    }
  }

  final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
    self =>

    private val tail = new AtomicReference(new Node[A]())
    private val suspended = new AtomicInteger(1)
    private val head = new AtomicReference(tail.get)

    /** Alias for `apply` */
    def !(a: A) {
      val n = new Node(a)
      head.getAndSet(n).lazySet(n)
      trySchedule()
    }

    /** Pass the message `a` to the mailbox of this actor */
    def apply(a: A) {
      this ! a
    }

    def contramap[B](f: B => A): Actor[B] =
      new Actor[B](strategy)((b: B) => (this ! f(b)), onError)

    private def trySchedule() {
      if (suspended.compareAndSet(1, 0)) schedule()
    }

    private def schedule() {
      strategy(act())
    }

    private def act() {
      val t = tail.get
      val n = batchHandle(t, 1024)
      if (n ne t) {
        n.a = null.asInstanceOf[A]
        tail.lazySet(n)
        schedule()
      } else {
        suspended.set(1)
        if (n.get ne null) trySchedule()
      }
    }

    @tailrec
    private def batchHandle(t: Node[A], i: Int): Node[A] = {
      val n = t.get
      if (n ne null) {
        try {
          handler(n.a)
        } catch {
          case ex: Throwable => onError(ex)
        }
        if (i > 0) batchHandle(n, i - 1) else n
      } else t
    }
  }

  private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

  object Actor {

    /** Create an `Actor` backed by the given `ExecutorService`. */
    def apply[A](es: ExecutorService = Executors.newCachedThreadPool())(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
      new Actor(Strategy.sequential)(handler, onError)
  }

  /**
    * Provides a function for evaluating expressions, possibly asynchronously.
    * The `apply` function should typically begin evaluating its argument
    * immediately. The returned thunk can be used to block until the resulting `A`
    * is available.
    */
  trait Strategy {
    def apply[A](a: => A): () => A
  }

  object Strategy {
    /**
      * A `Strategy` which begins executing its argument immediately in the calling thread.
      */
    def sequential: Strategy = new Strategy {
      def apply[A](a: => A): () => A = {
        val r = a
        () => r
      }
    }
  }
}
