package com.yevhenii.utils

import cats.effect.IO

object IoUtils {

  implicit class richIO[A](subject: IO[A]) {
    def peek[B](sideEffectIO: A => IO[B]): IO[A] = {
      subject.flatMap(x => sideEffectIO(x).map(_ => x))
    }
  }
}
