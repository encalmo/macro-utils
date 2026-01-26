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

  test("visit enum's cases 1") {
    assertEquals(
      testVisitMethod(entity1),
      List(
        "case _: Dog =>",
        "case Cow =>"
      )
    )
  }

  test("visit enum's cases 2") {
    assertEquals(
      testVisitMethod(entity2),
      List(
        "case _: Dog =>",
        "case Cow =>"
      )
    )
  }

  test("visit enum's cases 3") {
    assertEquals(
      testVisitMethod(entity3),
      List(
        "case _: Boeing =>",
        "case _: Airbus =>"
      )
    )
  }

  test("visit enum's cases 4") {
    assertEquals(
      testVisitMethod(entity4),
      List(
        "case _: Boeing =>",
        "case _: Airbus =>"
      )
    )
  }

  test("visit not an enum or sealed ADT") {
    assertEquals(
      testVisitMethod("not an enum or sealed ADT"),
      Nil
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

}
