package com.yevhenii.execution

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import annotation.tailrec
import cats.Monad

object Execution {

  case class BiFunctionWithComplexity[A, B, C](f: (A, B) => C, complexity: Int) extends ((A, B) => C) {
    override def apply(v1: A, v2: B): C = f(v1, v2)
  }

  trait Future[+A] {
    def apply(k: A => Unit): Unit
  }

  case class Flow[+A](f: Executor => Future[A], complexity: Int) extends (Executor => Future[A]) {
    override def apply(v1: Executor): Future[A] = f(v1)
  }

  object Flow {

//    implicit def conversion[A](f: Executor => Future[A]): Flow[A] = new Flow[A](f)

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

    def unit[A](a: A): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }, 1
    )

    def unit[A](a: A, complexity: Int): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      },
      complexity
    )

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }, 1
    )


    def delay[A](a: => A, complexity: Int): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      },
      complexity
    )

    def fork[A](a: => Flow[A]): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb), a.complexity)
      },
      a.complexity
    )

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(k: A => Unit) = f(k)
      }, 1
    )

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: Executor)(r: => Unit, complexity: Int): Unit =
      es.submit(new Callable[Unit] { def call = r }, complexity)

    def map2[A,B,C](p: Flow[A], p2: Flow[B])(f: (A,B) => C): Flow[C] = Flow (
      (es: Executor) => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[(A, Int), (B, Int)]]() {
            case Left((a, complexity)) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)), complexity)
              else ar = Some(a)
            case Right((b, complexity)) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)), complexity)
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a, p.complexity))
          p2(es)(b => combiner ! Right(b, p2.complexity))
        }
      }, 1 // todo incorrect complexity
    )

    // specialized version of `map`
    def map[A,B](p: Flow[A])(f: A => B): Flow[B] = Flow (
      (es: Executor) => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es)(cb(f(a)), p.complexity))
      }, p.complexity
    )

    def lazyUnit[A](a: => A): Flow[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Flow[B] =
      a => lazyUnit(f(a))

    def choice[A](p: Flow[Boolean])(t: Flow[A], f: Flow[A]): Flow[A] = Flow (
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es)(t(es)(cb), t.complexity)
            else eval(es)(f(es)(cb), f.complexity)
          }
      },
      p.complexity
    )

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] =
      flatMap(p)(f)

    def flatMap[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] = Flow (
      (es: Executor) => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      },
      p.complexity
    )

    implicit val flowMonad: Monad[Flow] = new Monad[Flow] {
      override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Flow[Either[A, B]]): Flow[B] = {
        f(a).flatMap {
          case Left(l) => tailRecM(l)(f)
          case Right(value) => Flow.unit(value)
        }
      }

      override def pure[A](x: A): Flow[A] = Flow.unit(x)

      override def map2[A, B, Z](fa: Flow[A], fb: Flow[B])(f: (A, B) => Z): Flow[Z] = Flow.map2(fa, fb)(f)
    }

    implicit def toParOps[A](p: Flow[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Flow[A]) {
      def map[B](f: A => B): Flow[B] = Flow.map(p)(f)
      def map2[B,C](b: Flow[B])(f: (A,B) => C): Flow[C] = Flow.map2(p,b)(f)
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
//    /** Create an `Actor` backed by the given `ExecutorService`. */
//    def apply[A](es: Executor)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
//      new Actor(Strategy.fromExecutor(es))(handler, onError)
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
//
//    /**
//      * We can create a `Strategy` from any `ExecutorService`. It's a little more
//      * convenient than submitting `Callable` objects directly.
//      */
//    def fromExecutor(es: Executor): Strategy = new Strategy {
//      def apply[A](a: => A): () => A = {
//        val promise = Promise[A]()
//        es.submit { new Callable[Unit] { def call = promise.trySuccess(a) } }
//        () => Await.result(promise.future, Duration.Inf)
//      }
//    }

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
