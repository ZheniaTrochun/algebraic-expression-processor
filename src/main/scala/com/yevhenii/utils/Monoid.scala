package com.yevhenii.utils

trait Monoid[A] {
  def unit: A

  def combine(a: A, b: A): A

  def |+|(a: A, b: A): A = combine(a, b)
}

object Monoid {
  def unit[A](implicit m: Monoid[A]): A = m.unit
  def combine[A](a: A, b: A)(implicit m: Monoid[A]): A = m.combine(a, b)
  def |+|[A](a: A, b: A)(implicit m: Monoid[A]): A = m.combine(a, b)
}

object Instances {
  implicit val stringMonoid = new Monoid[String] {
    def unit: String = ""
    def combine(a: String, b: String): String = a + b
  }

  implicit def listMonoid[A](implicit m: Monoid[A]): Monoid[List[A]] = new Monoid[List[A]] {
    def unit: List[A] = List.empty
    def combine(a: List[A], b: List[A]): List[A] = a ::: b
  }
}