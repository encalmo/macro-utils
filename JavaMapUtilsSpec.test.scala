package org.encalmo.utils

import JavaMapUtilsTestMacro.*

class JavaMapUtilsSpec extends munit.FunSuite {

  test("testMaybeVisitJavaMap") {
    val map = new java.util.HashMap[String, Int]()
    map.put("key1", 1)
    map.put("key2", 2)
    map.put("key3", 3)
    val result = testMaybeVisitJavaMap(map)
    assert(
      result.contains("key2 -> 2")
        && result.contains("key3 -> 3")
        && result.contains("key1 -> 1")
    )
  }
}
