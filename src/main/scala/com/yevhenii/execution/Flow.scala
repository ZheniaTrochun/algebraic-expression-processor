package com.yevhenii.execution

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import annotation.tailrec
import cats.Monad

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration

object Execution {

  trait Future[+A] {
    def apply(k: A => Unit): Unit
  }

//  type Flow[+A] = Executor => Future[A]
  class Flow[+A] (f: Executor => Future[A]) extends (Executor => Future[A]) {
    override def apply(v1: Executor): Future[A] = f(v1)
  }

  object Flow {


    implicit def conversion[A](f: Executor => Future[A]): Flow[A] = new Flow[A](f)

    def run[A](es: Executor)(p: Flow[A]): A = {
      val ref = new AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a =>
        ref.set(a)
        latch.countDown()
      } // Asynchronously set the result, and decrement the latch
      latch.await() // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Flow[A] =
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Flow[A] =
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Flow[A]): Flow[A] =
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    def async[A](f: (A => Unit) => Unit): Flow[A] = (es: Executor) => new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: Executor)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    //todo perform without actor
    def map2[A,B,C](p: Flow[A], p2: Flow[B])(f: (A,B) => C): Flow[C] =
      (es: Executor) => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
//          val combiner = Actor[Either[A,B]](es) {
          val combiner = Actor[Either[A,B]]() {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A,B](p: Flow[A])(f: A => B): Flow[B] =
      (es: Executor) => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): Flow[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Flow[B] =
      a => lazyUnit(f(a))

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Flow[Boolean])(t: Flow[A], f: Flow[A]): Flow[A] =
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { b =>
            if (b) eval(es) { t(es)(cb) }
            else eval(es) { f(es)(cb) }
          }
      }

    /* The code here is very similar. */
    def choiceN[A](p: Flow[Int])(ps: List[Flow[A]]): Flow[A] =
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es) { ind => eval(es) { ps(ind)(es)(cb) }}
      }

    def choiceViaChoiceN[A](a: Flow[Boolean])(ifTrue: Flow[A], ifFalse: Flow[A]): Flow[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K,V](p: Flow[K])(ps: Map[K,Flow[V]]): Flow[V] =
      (es: Executor) => new Future[V] {
        def apply(cb: V => Unit): Unit =
          p(es)(k => ps(k)(es)(cb))
      }

    /* `chooser` is usually called `flatMap` or `bind`. */
    def chooser[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] =
      flatMap(p)(f)

    def flatMap[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] =
      (es: Executor) => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      }

    def choiceViaFlatMap[A](p: Flow[Boolean])(f: Flow[A], t: Flow[A]): Flow[A] =
      flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Flow[Int])(choices: List[Flow[A]]): Flow[A] =
      flatMap(p)(i => choices(i))

    def join[A](p: Flow[Flow[A]]): Flow[A] =
      (es: Executor) => new Future[A] {
        def apply(cb: A => Unit): Unit =
          p(es)(p2 => eval(es) { p2(es)(cb) })
      }

    def joinViaFlatMap[A](a: Flow[Flow[A]]): Flow[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] =
      join(map(p)(f))

    implicit val flowMonad: Monad[Flow] = new Monad[Flow] {
      override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = fa.flatMap(f)
//      override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = fork { fa.flatMap(f) } // todo fork added

      override def tailRecM[A, B](a: A)(f: A => Flow[Either[A, B]]): Flow[B] = {
        f(a).flatMap {
          case Left(l) => tailRecM(l)(f)
          case Right(value) => Flow.unit(value)
        }
      }

      override def pure[A](x: A): Flow[A] = Flow.unit(x)

//      override def map2[A, B, Z](fa: Flow[A], fb: Flow[B])(f: (A, B) => Z): Flow[Z] = Flow.map2(fork(fa), fork(fb))(f)
      override def map2[A, B, Z](fa: Flow[A], fb: Flow[B])(f: (A, B) => Z): Flow[Z] = Flow.map2(fa, fb)(f)
    }

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Flow[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2` and `flatMap`
    class ParOps[A](p: Flow[A]) {
      def map[B](f: A => B): Flow[B] = Flow.map(p)(f)
//      def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
      def flatMap[B](f: A => Flow[B]): Flow[B] = Flow.flatMap(p)(f)
//      def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
    }
  }

  /*
   * Implementation is taken from `scalaz` library, with only minor changes. See:
   *
   * https://github.com/scalaz/scalaz/blob/scalaz-seven/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
   *
   * This code is copyright Andriy Plokhotnyuk, Runar Bjarnason, and other contributors,
   * and is licensed using 3-clause BSD, see LICENSE file at:
   *
   * https://github.com/scalaz/scalaz/blob/scalaz-seven/etc/LICENCE
   */

  /**
    * Processes messages of type `A`, one at a time. Messages are submitted to
    * the actor with the method `!`. Processing is typically performed asynchronously,
    * this is controlled by the provided `strategy`.
    *
    * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
    * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
    * the `strategy` runs the invocations of `handler` on separate threads. This is achieved because
    * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
    * location before suspending.
    *
    * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
    * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
    *
    * @see scalaz.concurrent.Promise for a use case.
    *
    * @param handler  The message handler
    * @param onError  Exception handler, called if the message handler throws any `Throwable`.
    * @param strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
    * @tparam A       The type of messages accepted by this actor.
    */
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

    /**
      * We can create a `Strategy` from any `ExecutorService`. It's a little more
      * convenient than submitting `Callable` objects directly.
      */
    def fromExecutor(es: Executor): Strategy = new Strategy {
      def apply[A](a: => A): () => A = {
        val promise = Promise[A]()
        es.submit { new Callable[Unit] { def call = promise.trySuccess(a) } }
        () => Await.result(promise.future, Duration.Inf)
      }
    }

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
//
//object ActorlessFuture {
//
////  import java.util.concurrent._
//
//  import java.util.concurrent.Future
//
//  object Flow {
//    type Flow[A] = Executor => Future[A]
//
//    def run[A](s: Executor)(a: Flow[A]): Future[A] = a(s)
//
//    /*
//    The implementation of `unit` does not use the `ExecutorService`, it simply returns a `Future` directly.
//    */
//    def unit[A](a: A): Flow[A] =
//      (es: Executor) => UnitFuture(a)
//
//    /* Simple future for wrapping a constant value. */
//    case class UnitFuture[A](get: A) extends Future[A] {
//      def isDone = true
//      def get(timeout: Long, units: TimeUnit) = get
//      def isCancelled = false
//      def cancel(evenIfRunning: Boolean): Boolean = false
//    }
//
//    /*
//    Notice this implementation does not evaluate the call to `f` in a separate logical thread. This is in keeping with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
//
//    This implementation does _not_ respect timeouts. In order to respect timeouts, we need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
//    */
//    def map2_simple[A,B,C](a: Flow[A], b: Flow[B])(f: (A,B) => C): Flow[C] =
//      (es: Executor) => {
//        val af = a(es)
//        val bf = b(es)
//        UnitFuture(f(af.get, bf.get))
//      }
//
//    /* This version respects timeouts. See `Map2Future` below. */
//    def map2[A,B,C](a: Flow[A], b: Flow[B])(f: (A,B) => C): Flow[C] =
//      es => {
//        val (af, bf) = (a(es), b(es))
//        Map2Future(af, bf, f)
//      }
//
//    /*
//    This is the simplest, most natural implementation, but there are some problems with it--for one, the outer `Callable` will block waiting for the 'inner' task to complete. Since this blocked thread occupies a thread in our thread pool or whatever resource backs the `ExecutorService`, this implies we're losing out on some potential parallelism (essentially, we are using two threads when one should do). This is a symptom of a more serious problem with the implementation that we'll discuss later in the chapter.
//    */
////    def fork_simple[A](a: => Flow[A]): Flow[A] =
////      es => {
////        val promise = Promise[A]()
////        es.submit(new Callable[Unit] { // todo fix unit callable
////          def call = a(es).get
////        })
////      }
//
//
//    /*
//    Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
//    */
//    case class Map2Future[A,B,C](a: Future[A], b: Future[B],
//                                 f: (A,B) => C) extends Future[C] {
//      var cache: Option[C] = None
//      def isDone = cache.isDefined
//      def isCancelled = a.isCancelled || b.isCancelled
//      def cancel(evenIfRunning: Boolean) =
//        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
//      def get = compute(Long.MaxValue)
//      def get(timeout: Long, units: TimeUnit): C =
//        compute(TimeUnit.MILLISECONDS.convert(timeout, units))
//
//      private def compute(timeoutMs: Long): C = cache match {
//        case Some(c) => c
//        case None =>
//          val start = System.currentTimeMillis
//          val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
//          val stop = System.currentTimeMillis; val at = stop-start
//          val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
//          cache = Some(f(ar, br))
//          cache.get
//      }
//    }
//
//    def lazyUnit[A](a: => A): Flow[A] =
//      (es: Executor) => fork(unit(a))(es)
//
//    def asyncF[A,B](f: A => B): A => Flow[B] =
//      a => fork(unit(f(a)))
//
//    def map[A,B](fa: Flow[A])(f: A => B): Flow[B] =
//      flatMap(fa)(asyncF(f))
////      map2(fa, unit(()))((a,_) => f(a))
//
//    def map2Async[A,B,C](a: Flow[A], b: Flow[B])(f: (A,B) => C): Flow[C] =
//      es => {
//        val (af, bf) = (a(es), b(es))
//        Map2Future(af, bf, f)
//      }
//
//    def delay[A](fa: => Flow[A]): Flow[A] =
//      es => fa(es)
//
//    /*
//    The correctness of this implementation requires only that the `ExecutorService` begins executing tasks in the order they are submitted. This enables us to safely call `innerF.get`. (You may want to try proving to yourself that this cannot deadlock)
//    */
//    def fork[A](p: => Flow[A]): Flow[A] = {
//      es => {
//        val latch = new CountDownLatch(1)
//        var result: Option[A] = None
//        var innerF: Future[A] = null
//        var resultF: Future[_] = null
//        es.submit(new Runnable {
//          def run = {
//            innerF = p(es)
//            es.submit(new Runnable {
//              def run = { result = Some(innerF.get); latch.countDown }
//            })
//          }
//        })
//        new Future[A] {
//          def get = { latch.await; result.get }
//          def get(timeout: Long, units: TimeUnit) = {
//            latch.await(timeout, units)
//            result.get
//          }
//          def isDone = latch.getCount == 0
//          def cancel(b: Boolean) = false
//          var isCancelled = false
//        }
//      }
//    }
//
//    /* `chooser` is usually called `flatMap` or `bind`. */
//    def flatMap[A,B](a: Flow[A])(choices: A => Flow[B]): Flow[B] =
//      es => choices(a(es).get)(es)
//
//    /*
//    This implementation is not safe for execution on bounded thread pools, and it also does not preserve timeouts. Can you see why? You may wish to try implementing a nonblocking version like was done for `fork`.
//    */
//    def join[A](a: Flow[Flow[A]]): Flow[A] =
//      es => a(es).get.apply(es)
//
//    def flatMapViaJoin[A,B](p: Flow[A])(f: A => Flow[B]): Flow[B] =
//      join(map(p)(f))
//    /* Gives us infix syntax for `Par`. */
//    implicit def toParOps[A](p: Flow[A]): ParOps[A] = new ParOps(p)
//
//    implicit val flowMonad: Monad[Flow] = new Monad[Flow] {
//      override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = fa.flatMap(f)
//
//      override def tailRecM[A, B](a: A)(f: A => Flow[Either[A, B]]): Flow[B] = {
//        f(a).flatMap {
//          case Left(l) => tailRecM(l)(f)
//          case Right(value) => Flow.unit(value)
//        }
//      }
//
//      override def pure[A](x: A): Flow[A] = Flow.unit(x)
//
//      override def map2[A, B, Z](fa: Flow[A], fb: Flow[B])(f: (A, B) => Z): Flow[Z] = Flow.map2(fa, fb)(f)
//    }
//
//    class ParOps[A](p: Flow[A]) {
//      def map[B](f: A => B): Flow[B] = Flow.map(p)(f)
//      def map2[B,C](b: Flow[B])(f: (A,B) => C): Flow[C] = Flow.map2(p,b)(f)
//      def flatMap[B](f: A => Flow[B]): Flow[B] = Flow.flatMap(p)(f)
//      def zip[B](b: Flow[B]): Flow[(A,B)] = p.map2(b)((_,_))
//    }
//  }
//}