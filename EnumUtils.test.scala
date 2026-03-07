package org.encalmo.utils

import EnumUtilsTestMacro.*

class EnumUtilsSpec extends munit.FunSuite {

  enum Entity {
    case Dog(name: String)
    case Cow
  }

  val entity1 = Entity.Dog("Rex")
  val entity2 = Entity.Cow
  val entity3: Planes = Boeing("747")
  val entity4: Planes = Airbus("A380")

  test("transform enum's cases") {
    assertEquals(
      testTransformToTermMethod(entity1),
      "case _: Dog => Dog(Rex)"
    )
    assertEquals(
      testTransformToTermMethod(entity2),
      "case Cow => Cow"
    )
    assertEquals(
      testTransformToTermMethod(entity3),
      "case _: Boeing => Boeing(747)"
    )
    assertEquals(
      testTransformToTermMethod(entity4),
      "case _: Airbus => Airbus(A380)"
    )
  }

  test("isEnumOrSealedADT") {
    assertEquals(testIsEnumOrSealedADT[Entity], true)
    assertEquals(testIsEnumOrSealedADT[Hobby], true)
    assertEquals(testIsEnumOrSealedADT[Hobby.Cooking.type], true)
    assertEquals(testIsEnumOrSealedADT[Cars], true)
    assertEquals(testIsEnumOrSealedADT[List[String]], true)
    assertEquals(testIsEnumOrSealedADT[Option[String]], true)
    assertEquals(testIsEnumOrSealedADT[NonEmptyTuple], true)
    // neg
    assertEquals(testIsEnumOrSealedADT[Hobby.Other], false)
    assertEquals(testIsEnumOrSealedADT[Cars.Other], false)
    assertEquals(testIsEnumOrSealedADT[Planes], true)
    assertEquals(testIsEnumOrSealedADT[Airbus], false)
    assertEquals(testIsEnumOrSealedADT[Boeing], false)
    assertEquals(testIsEnumOrSealedADT[Boats], false)
    assertEquals(testIsEnumOrSealedADT[SensitiveData[String]], false)
    assertEquals(testIsEnumOrSealedADT[Disability], false)
    assertEquals(testIsEnumOrSealedADT[PassportNumber], false)
    assertEquals(testIsEnumOrSealedADT[ImmigrationStatus], false)
    assertEquals(testIsEnumOrSealedADT[Name], false)
    assertEquals(testIsEnumOrSealedADT[Array[String]], false)
    assertEquals(testIsEnumOrSealedADT[Int], false)
    assertEquals(testIsEnumOrSealedADT[Boolean], false)
    assertEquals(testIsEnumOrSealedADT[Double], false)
    assertEquals(testIsEnumOrSealedADT[Float], false)
    assertEquals(testIsEnumOrSealedADT[Long], false)
    assertEquals(testIsEnumOrSealedADT[Short], false)
    assertEquals(testIsEnumOrSealedADT[Byte], false)
    assertEquals(testIsEnumOrSealedADT[Char], false)
    assertEquals(testIsEnumOrSealedADT[(String, Int)], false)
    assertEquals(testIsEnumOrSealedADT[EmptyTuple], false)
  }

  test("visitTermless") {
    assertEquals(visitTermless[Entity], "case class Dog, value case Cow")
    assertEquals(visitTermless[Planes], "case class Boeing, case class Airbus")
    assertEquals(
      visitTermless[Hobby],
      "value case Reading, value case Swimming, value case Cycling, value case Cooking, case class Other"
    )
    assertEquals(
      visitTermless[Cars],
      "value case Ford, value case Toyota, value case Honda, value case Nissan, case class Other"
    )
    assertEquals(visitTermless[Boats], "")
    assertEquals(
      visitTermless[Benefit],
      "value case ChildBenefit, value case UniversalCredit, value case JobSeekersAllowance, value case EmploymentSupportAllowance, value case HousingBenefit, value case PensionCredit, case class Other"
    )
    assertEquals(
      visitTermless[MaritalStatus],
      "value case Single, case class CivilPartnership, case class Married, case class Divorced, case class Widowed"
    )
  }

}
