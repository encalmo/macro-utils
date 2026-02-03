package org.encalmo.utils

import EitherUtilsTestMacro.*

class EitherUtilsSpec extends munit.FunSuite {

  test("testBuildMatchTerm and apply Left of String") {
    assertEquals(
      testBuildMatchTerm[String, Int](Left("foo")),
      "Left(foo)"
    )
  }

  test("testBuildMatchTerm and apply Left of Int") {
    assertEquals(
      testBuildMatchTerm[Int, Int](Left(123)),
      "Left(123)"
    )
  }

  test("testBuildMatchTerm and apply Right of Int") {
    assertEquals(
      testBuildMatchTerm[String, Int](Right(123)),
      "Right(123)"
    )
  }

  test("testBuildMatchTerm and apply Right of Int") {
    assertEquals(
      testBuildMatchTerm[String, Int](Right(123)),
      "Right(123)"
    )
  }

}
