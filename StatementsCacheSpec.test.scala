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

  test("testCreateMethodOfUnit") {
    val result = testCreateMethod("_foo")
    assertEquals(result, "ouch_foo")
  }

  test("testCreateLargeMethod") {
    val result = testCreateLargeMethod(".foo")
    assertEquals(result, "ouch.foo10")
  }
}
