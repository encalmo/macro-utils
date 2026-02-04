package org.encalmo.utils
import StatementsCacheTestMacro.*

class StatementsCacheSpec extends munit.FunSuite {

  test("testCreateNestedScope") {
    val result = testCreateNestedScope()
    assertEquals(result, "Outer!, Nested!, Outer!")
  }
}
