package com.scalamonthly

import scala.util.Try
import scala.util.Failure
import cats.data.NonEmptyList

object FundamentalsSpec extends weaver.SimpleIOSuite {

  import fundamentals._

  pureTest("one") {
    val result = one(100L)
    expect.same(result, "100")
  }

  pureTest("two") {
    val result = two[Option, String]("hello")
    expect.same(result, Some("hello"))
  }

  pureTest("three") {
    val error = new Exception("test")
    val result = three[Try](error)
    expect.same(result, Failure(error))
  }

  pureTest("four") {
    implicit val combineString: Combine[String] = new Combine[String] {
      def combine(lhs: String, rhs: String): String = lhs + rhs
    }
    val input = NonEmptyList.of("h", "e", "l", "l", "o")
    val result = four[String](input)
    expect.same(result, "hello")
  }

  pureTest("five") {
    val input = NonEmptyList.of(1, 2, 3, 4, 5)
    val result = five(input)
    expect.same(result, 15)
  }

  pureTest("six") {
    implicit val mappableList: Mappable[List] = new Mappable[List] {
      def map[A, B](in: List[A], f: A => B): List[B] = in.map(f)
    }
    val result = six(List(1, 2, 3, 4, 5))
    expect.same(result, List("1", "2", "3", "4", "5"))
  }

}