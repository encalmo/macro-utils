package org.encalmo.utils

import OpaqueUtilsTestMacro.*
import java.time.LocalDate

class OpaqueTypeUtilsSpec extends munit.FunSuite {

  val passportNumber = PassportNumber("1234567890")
  val disability = Disability("blindness")
  val driverLicense = DriverLicense("1234567890", LocalDate.now())
  val skills = Skills("Java", "Scala", "Python")

  test("transformToMatchExpression opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testTransformToMatchExpression(driverLicense),
      List(
        "opaque type with an upper bound of Document"
      )
    )
  }

  test("transformToMatchExpression opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testTransformToMatchExpression(skills),
      List(
        "opaque type with an upper bound of Set[String]"
      )
    )
  }

  test("transformToMatchExpression opaque type without upper bound - PassportNumber") {
    assertEquals(
      testTransformToMatchExpression(passportNumber),
      List(
        "not an opaque type PassportNumber"
      )
    )
  }

  test("transformToMatchExpression opaque type without upper bound - Disability") {
    assertEquals(
      testTransformToMatchExpression(disability),
      List(
        "not an opaque type Disability"
      )
    )
  }

  test("transformToMatchExpression not an opaque type") {
    assertEquals(
      testTransformToMatchExpression("not an opaque type"),
      List("not an opaque type String")
    )
  }

  test("transformToMatchExpression opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testVisit(driverLicense),
      "opaque type with an upper bound of Document"
    )
  }

  test("transformToMatchExpression opaque type with an upper bound - DriverLicense") {
    assertEquals(
      testVisit(skills),
      "opaque type with an upper bound of Set[String]"
    )
  }

  test("transformToMatchExpression opaque type without upper bound - PassportNumber") {
    assertEquals(
      testVisit(passportNumber),
      "not an opaque type PassportNumber"
    )
  }

  test("transformToMatchExpression opaque type without upper bound - Disability") {
    assertEquals(
      testVisit(disability),
      "not an opaque type Disability"
    )
  }

  test("transformToMatchExpression not an opaque type") {
    assertEquals(
      testVisit("not an opaque type"),
      "not an opaque type String"
    )
  }
}
