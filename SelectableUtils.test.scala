package org.encalmo.utils

import SelectableUtilsTestMacro.*

class SelectableUtilsSpec extends munit.FunSuite {

  val entity = FactsRow()

  test("visit record") {
    assertEquals(
      testMaybeVisitSelectable(entity),
      "name: String = John Doe, age: Int = 30, email: String = john.doe@example.com"
    )
  }

  test("transform record") {
    assertEquals(
      testMaybeTransformSelectableIntoBlockOfUnit(entity),
      "name: String, age: Int, email: String"
    )
  }

  test("visit not a record") {
    assertEquals(
      testMaybeVisitSelectable("not a record"),
      ""
    )
  }

  test("transform not a record") {
    assertEquals(
      testMaybeTransformSelectableIntoBlockOfUnit("not a record"),
      ""
    )
  }

}
