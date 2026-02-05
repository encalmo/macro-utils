package org.encalmo.utils

import JavaIterableTestMacro.*

class JavaIterableSpec extends munit.FunSuite {

  test("testMaybeVisitJavaIterable") {
    val list = java.util.List.of(1, 2, 3)
    assertEquals(
      testMaybeVisitJavaIterable(list),
      "1, 2, 3"
    )
  }
}
