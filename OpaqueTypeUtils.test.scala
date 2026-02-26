package org.encalmo.utils

import OpaqueUtilsTestMacro.*
import java.time.LocalDate

class OpaqueTypeUtilsSpec extends munit.FunSuite {

  val passportNumber = PassportNumber("1234567890")
  val disability = Disability("blindness")
  val driverLicense = DriverLicense("1234567890", LocalDate.now())
  val skills = Skills("Java", "Scala", "Python")
  val nationalID = NationalID(Document("1234567890", LocalDate.now()))
  val fishingLicense = FishingLicense("1234567890", LocalDate.now())

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

  test("visit opaque type with an unapply method - NationalID") {
    assertEquals(
      testVisit(nationalID),
      "opaque type with an upper bound of Document"
    )
  }

  test("visit opaque type with an apply method - FishingLicense") {
    assertEquals(
      testVisit(fishingLicense),
      "opaque type with an upper bound of Document"
    )
  }

  test("visit opaque type without upper bound - PassportNumber") {
    assertEquals(
      testVisit(passportNumber),
      "opaque type with an upper bound of String"
    )
  }

  test("visit opaque type without upper bound - Disability") {
    assertEquals(
      testVisit(disability),
      "opaque type with an upper bound of SensitiveData"
    )
  }

  test("visit not an opaque type") {
    assertEquals(
      testVisit("not an opaque type"),
      "not an opaque type String"
    )
  }

  test("find base type from unapply - DriverLicense") {
    assertEquals(
      testFindBaseTypeFromUnapply[DriverLicense],
      "<none>"
    )
  }

  test("find base type from unapply - FishingLicense") {
    assertEquals(
      testFindBaseTypeFromUnapply[FishingLicense],
      "<none>"
    )
  }

  test("find base type from unapply - NationalID") {
    assertEquals(
      testFindBaseTypeFromUnapply[NationalID],
      "Document"
    )
  }

  test("find base type from unapply - FishingLicense") {
    assertEquals(
      testFindBaseTypeFromUnapply[FishingLicense],
      "<none>"
    )
  }

  test("find base type from apply - DriverLicense") {
    assertEquals(
      testFindBaseTypeFromApply[DriverLicense],
      "Document"
    )
  }

  test("find base type from apply - FishingLicense") {
    assertEquals(
      testFindBaseTypeFromApply[FishingLicense],
      "Document"
    )
  }

  test("find base type from apply - NationalID") {
    assertEquals(
      testFindBaseTypeFromApply[NationalID],
      "Document"
    )
  }

  test("find base type from apply - Disability") {
    assertEquals(
      testFindBaseTypeFromApply[Disability],
      "SensitiveData"
    )
  }

}
