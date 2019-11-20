package com.yevhenii.utils

import cats.Monad

import scala.language.higherKinds

trait TraversableTree[T] {
  def traverse[F[_]: Monad](t: T)(f: T => F[T]): F[T]
}

object TraversableTree {
  def apply[T](implicit instance: TraversableTree[T]): TraversableTree[T] = instance
}
