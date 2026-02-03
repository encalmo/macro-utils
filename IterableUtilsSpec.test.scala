package org.encalmo.utils

import IterableUtilsTestMacro.*

class IterableUtilsSpec extends munit.FunSuite {

  test("testBuildIterableLoop and apply to List") {
    assertEquals(
      testBuildIterableLoop[Int](List(1, 2, 3)),
      "1, 2, 3"
    )
  }

  test("testCreateStaticList") {
    assertEquals(
      testCreateStaticList("123567890"),
      List("1", "2", "3", "5", "6", "7", "8", "9", "0")
    )
  }
}
