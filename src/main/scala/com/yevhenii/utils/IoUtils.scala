package com.yevhenii.utils

import cats.effect.IO

object IoUtils {

  implicit class richIO[A](subject: IO[A]) {
    def peek(sideEffectIO: A => IO[Unit]): IO[A] = {
      subject.flatMap(x => sideEffectIO(x).map(_ => x))
    }
  }
}
