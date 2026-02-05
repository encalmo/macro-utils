package org.encalmo.utils

import IterableUtilsTestMacro.*

class IterableUtilsSpec extends munit.FunSuite {

  test("testBuildIterableLoop and apply to List") {
    assertEquals(
      testBuildIterableLoop((List(1, 2, 3), List("one", "two", "three"))),
      "1, 2, 3, one, two, three"
    )
  }

  test("testCreateStaticList") {
    assertEquals(
      testCreateStaticList("123567890"),
      List("1", "2", "3", "5", "6", "7", "8", "9", "0")
    )
  }
}
