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
      testVisitMethod(driverLicense),
      List(
        "opaque type with an upper bound of Document"
      )
    )
  }

  test("visit opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testVisitMethod(skills),
      List(
        "opaque type with an upper bound of Set[String]"
      )
    )
  }

  test("visit opaque type without upper bound - PassportNumber") {
    assertEquals(
      testVisitMethod(passportNumber),
      List(
        "not an opaque type PassportNumber"
      )
    )
  }

  test("visit opaque type without upper bound - Disability") {
    assertEquals(
      testVisitMethod(disability),
      List(
        "not an opaque type Disability"
      )
    )
  }

  test("visit not an opaque type") {
    assertEquals(
      testVisitMethod("not an opaque type"),
      List("not an opaque type String")
    )
  }
}
