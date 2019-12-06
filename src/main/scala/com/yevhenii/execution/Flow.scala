package com.yevhenii.execution

import cats.Monad

trait Flow[A] {

  type Err = String

  def run: Either[Err, A]

  def join[B](other: Flow[B]): Flow[(A, B)]

  def flatMap[B](f: A => Flow[B]): Flow[B]

  def map[B](f: A => B): Flow[B]
}

// todo
object Flow {
  implicit val flowMonad: Monad[Flow] = new Monad[Flow] {
    override def flatMap[A, B](fa: Flow[A])(f: A => Flow[B]): Flow[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Flow[Either[A, B]]): Flow[B] = ???

    override def pure[A](x: A): Flow[A] = ???
  }
}
