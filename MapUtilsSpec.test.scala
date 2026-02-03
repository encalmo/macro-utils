package org.encalmo.utils

import MapUtilsTestMacro.*

class MapUtilsSpec extends munit.FunSuite {

  test("testBuildMapLoop") {
    assertEquals(
      testBuildMapLoop[Int, String](Map(1 -> "one", 2 -> "two", 3 -> "three")),
      "1 -> one, 2 -> two, 3 -> three"
    )
  }

}
