package org.encalmo.utils

import JavaRecordUtilsTestMacro.*

class JavaRecordUtilsSpec extends munit.FunSuite {

  val entity = Order(
    "1234567890",
    "John Doe",
    java.util.List.of(100, 200, 300),
    java.math.BigDecimal(1000)
  )

  test("collect record") {
    assertEquals(
      testCollectMethod(entity),
      List(
        "id: String = 1234567890",
        "customerId: String = John Doe",
        "items: List[Integer] = [100, 200, 300]",
        "total: BigDecimal = 1000"
      )
    )
  }

  test("collect not a record") {
    assertEquals(
      testCollectMethod("not a record"),
      Nil
    )
  }

  test("visit record") {
    assertEquals(
      testVisitMethod(entity),
      "id: String = 1234567890, customerId: String = John Doe, items: List[Integer] = [100, 200, 300], total: BigDecimal = 1000"
    )
  }
}
