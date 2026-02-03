package org.encalmo.utils

import org.encalmo.utils.OptionUtilsTestMacro.*

class OptionUtilsSpec extends munit.FunSuite {

  test("testBuildMatchTerm and apply Some of String") {
    assertEquals(
      testBuildMatchTerm[String](Some("foo")),
      "foo"
    )
  }

  test("testBuildMatchTerm and apply Some of Int") {
    assertEquals(
      testBuildMatchTerm[Int](Some(123)),
      "123"
    )
  }

  test("testBuildMatchTerm and apply Some of Int") {
    case class Address(street: String, city: String, country: String)
    assertEquals(
      testBuildMatchTerm[Address](Some(Address("123 Main St", "Anytown", "USA"))),
      "Address(123 Main St,Anytown,USA)"
    )
  }

  test("testBuildMatchTerm and apply None") {
    assertEquals(
      testBuildMatchTerm[String](None),
      "<none>"
    )
  }

}
