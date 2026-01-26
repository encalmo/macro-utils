package org.encalmo.utils

import TypeNameUtils.*

class TypeNameUtilsSpec extends munit.FunSuite {

  test("typeName") {
    assertEquals(typeName[TestEnum], "TestEnum")
    assertEquals(typeName[Order], "Order")
    assertEquals(typeName[Status], "Status")
    assertEquals(typeName[Benefit], "Benefit")
    assertEquals(typeName[Cars], "Cars")
    assertEquals(typeName[Boats], "Boats")
    assertEquals(typeName[Planes], "Planes")
    assertEquals(typeName[Airbus], "Airbus")
    assertEquals(typeName[Boeing], "Boeing")
    assertEquals(typeName[Hobby], "Hobby")
    assertEquals(typeName[Hobby.Other], "Other")
    assertEquals(typeName[Hobby.Cooking.type], "Cooking")
    assertEquals(typeName[Hobby.Other], "Other")
    assertEquals(typeName[SensitiveData[String]], "SensitiveData")
    assertEquals(typeName[Disability], "Disability")
    assertEquals(typeName[DriverLicense], "DriverLicense")
    assertEquals(typeName[PassportNumber], "PassportNumber")
    assertEquals(typeName[Option[String]], "Option")
    assertEquals(typeName[List[String]], "List")
    assertEquals(typeName[Array[String]], "Array")
    assertEquals(typeName[Hobby], "Hobby")
    assertEquals(typeName[PassportNumber], "PassportNumber")
    assertEquals(typeName[ImmigrationStatus], "ImmigrationStatus")
    assertEquals(typeName[String], "String")
    assertEquals(typeName[Int], "Int")
    assertEquals(typeName[Boolean], "Boolean")
    assertEquals(typeName[Double], "Double")
    assertEquals(typeName[Float], "Float")
    assertEquals(typeName[Long], "Long")
    assertEquals(typeName[Short], "Short")
    assertEquals(typeName[Byte], "Byte")
    assertEquals(typeName[Char], "Char")
  }
}
