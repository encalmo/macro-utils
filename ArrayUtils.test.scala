package org.encalmo.utils

import ArrayUtilsTestMacro.*

class ArrayUtilsSpec extends munit.FunSuite {

  test("testBuildArrayLoop") {
    assertEquals(
      testBuildArrayLoop(Array(1, 2, 3)),
      "1 at 0, 2 at 1, 3 at 2"
    )
  }

  test("testCreateArrayViaList") {
    val array: Array[Int] = testCreateArrayViaList(List(1, 2, 3))
    assert(
      array.sameElements(Array(1, 2, 3))
    )
  }
}
