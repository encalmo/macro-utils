package org.encalmo.utils

import TermUtilsTestMacro.*

class TermUtilsSpec extends munit.FunSuite {

  test("is literal constant") {
    assertEquals(testIsLiteralConstant(5), true)
    val x = 5
    assertEquals(testIsLiteralConstant(x), false)
  }

  test("is null constant") {
    assertEquals(testIsNullConstant(null), true)
    val x = null
    assertEquals(testIsNullConstant(x), false)
  }
}
