package org.encalmo.utils

import NamedTupleUtilsTestMacro.*

class NamedTupleUtilsSpec extends munit.FunSuite {

  val tuple = (1, "two", List(1d, 2d))
  val namedTuple = (name = "John", age = 30, email = List("john.doe@example.com", "jane.doe@example.com"))

  test("Visit named tuple") {
    assertEquals(
      testVisitMethod(namedTuple),
      List(
        "named tuple element name: String = John",
        "named tuple element age: Int = 30",
        "named tuple element email: List[String] = List(john.doe@example.com, jane.doe@example.com)"
      )
    )
  }

  test("Visit named tuple without value term") {
    assertEquals(
      testVisitTermlessMethod[namedTuple.type],
      List(
        "named tuple element name: String",
        "named tuple element age: Int",
        "named tuple element email: List[String]"
      )
    )
  }

  test("Collect named tuple") {
    assertEquals(
      testCollectMethod(namedTuple),
      List(
        "named tuple element name: String = John",
        "named tuple element age: Int = 30",
        "named tuple element email: List[String] = List(john.doe@example.com, jane.doe@example.com)"
      )
    )
  }

  test("isNamedTuple") {
    assertEquals(testIsNamedTuple[namedTuple.type], true)
    // neg
    assertEquals(testIsNamedTuple[tuple.type], false)
    assertEquals(testIsNamedTuple[Option[String]], false)
    assertEquals(testIsNamedTuple[List[String]], false)
    assertEquals(testIsNamedTuple[Array[String]], false)
    assertEquals(testIsNamedTuple[Int], false)
    assertEquals(testIsNamedTuple[Boolean], false)
  }

}
