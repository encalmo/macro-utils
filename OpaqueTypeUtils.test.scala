package org.encalmo.utils

import OpaqueUtilsTestMacro.*
import java.time.LocalDate

class OpaqueTypeUtilsSpec extends munit.FunSuite {

  val passportNumber = PassportNumber("1234567890")
  val disability = Disability("blindness")
  val driverLicense = DriverLicense("1234567890", LocalDate.now())
  val skills = Skills("Java", "Scala", "Python")

  test("visit opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testVisit(driverLicense),
      "opaque type with an upper bound of Document"
    )
  }

  test("visit opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testVisit(skills),
      "opaque type with an upper bound of Set[String]"
    )
  }

  test("visit opaque type without upper bound - PassportNumber") {
    assertEquals(
      testVisit(passportNumber),
      "not an opaque type PassportNumber"
    )
  }

  test("visit opaque type without upper bound - Disability") {
    assertEquals(
      testVisit(disability),
      "not an opaque type Disability"
    )
  }

  test("visit not an opaque type") {
    assertEquals(
      testVisit("not an opaque type"),
      "not an opaque type String"
    )
  }
}
