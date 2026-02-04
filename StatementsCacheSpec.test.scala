package org.encalmo.utils
import StatementsCacheTestMacro.*

class StatementsCacheSpec extends munit.FunSuite {

  test("testCreateNestedScope") {
    val result = testCreateNestedScope()
    assertEquals(result, "Outer!, huhu, puff, few, Outer!")
  }

  test("testCreateEmptyNestedScope") {
    val result = testCreateEmptyNestedScope()
    assertEquals(result, "Outer!, Outer!")
  }
}
