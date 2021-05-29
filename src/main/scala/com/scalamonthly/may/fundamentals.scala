package com.scalamonthly.may

import cats.data.NonEmptyList
import cats.{Applicative, ApplicativeThrow, Show}

object fundamentals {

  /**
    * Use the Show type class to turn `a` into a String.
    */
  def one[A: Show](a: A): String = Show[A].show(a)

  /**
    * Lift `input` into `F` using `Applicative`.
    */
  def two[F[_]: Applicative, A](input: A): F[A] = Applicative[F].pure(input)

  /**
    * Raise the `error` inside of `F` using `ApplicativeThrow`.
    */
  def three[F[_]: ApplicativeThrow](error: Throwable): F[Unit] = ApplicativeThrow[F].raiseError(error)

  trait Combine[A] {
    def combine(lhs: A, rhs: A): A
  }

  object Combine {
    def apply[A](implicit combine: Combine[A]): Combine[A] = combine
  }

  /**
    * Use the `Combine` type class to combine all `A` in `l` into a single `A`.
    */
  def four[A: Combine](l: NonEmptyList[A]): A = l.tail.foldLeft(l.head)((a1, a2) => Combine[A].combine(a1, a2))

  /**
    * Using the `Combine` type class AND the function `four` above,
    * add all `Int`s in `ints` together.
    */
  def five(ints: NonEmptyList[Int]): Int = {
    implicit val sumCombine = new Combine[Int]{
      def combine(a:Int, b:Int):Int = a + b
    }
    four(ints)
  }

  trait Mappable[F[_]] {
    def map[A, B](in: F[A], f: A => B): F[B]
  }

  object Mappable {
    def apply[F[_]](implicit m: Mappable[F]): Mappable[F] = m
  }

  /**
    * Use `Mappable` to apply `toString` to every `Int` inside of the provided `F`.
    */
  def six[F[_]: Mappable](in: F[Int]): F[String] = Mappable[F].map(in, Integer.toString)

}
