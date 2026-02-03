package org.encalmo.utils

import TupleUtilsTestMacro.*

class TupleUtilsSpec extends munit.FunSuite {

  val tuple = (1, "two", List(1d, 2d))
  val namedTuple = (name = "John", age = 30, email = List("john.doe@example.com", "jane.doe@example.com"))

  test("Collect ordinary tuple") {
    assertEquals(
      testCollectMethod(tuple),
      List(
        "tuple element at 0: Int = 1",
        "tuple element at 1: String = two",
        "tuple element at 2: List[Double] = List(1.0, 2.0)"
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

  test("Collect not a tuple") {
    assertEquals(
      testCollectMethod("not a tuple"),
      Nil
    )
  }

  test("Visit ordinary tuple") {
    assertEquals(
      testVisitMethod(tuple),
      List(
        "tuple element at 0: Int = 1",
        "tuple element at 1: String = two",
        "tuple element at 2: List[Double] = List(1.0, 2.0)"
      )
    )
  }

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

  test("isTuple") {
    assertEquals(testIsTuple[tuple.type], true)
    // neg
    assertEquals(testIsTuple[namedTuple.type], false)
    assertEquals(testIsTuple[Option[String]], false)
    assertEquals(testIsTuple[List[String]], false)
    assertEquals(testIsTuple[Array[String]], false)
    assertEquals(testIsTuple[Int], false)
    assertEquals(testIsTuple[Boolean], false)
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

  test("createTuple") {
    assertEquals(testCreateTuple[Int, String](1, "two"), (1, "two"))
  }

}
