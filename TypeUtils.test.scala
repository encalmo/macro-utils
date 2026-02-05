package org.encalmo.utils

class TypeUtilsSpec extends munit.FunSuite {

  test("testTypeReprIsPrimitiveOrStringOrBigDecimal") {
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[String], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Int], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Boolean], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Double], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Float], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Long], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Char], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Byte], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Short], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[BigDecimal], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[BigDecimal], true)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[java.math.BigDecimal], true)
    // neg
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Unit], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[java.math.BigInteger], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[BigInt], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Option[String]], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[List[String]], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[Array[String]], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[NonEmptyTuple], false)
    assertEquals(TypeUtilsTestMacro.testTypeReprIsPrimitiveOrStringOrBigDecimal[EmptyTuple], false)

  }

  test("isCaseClass") {
    assertEquals(TypeUtils.isCaseClass[Hobby.Other], true)
    assertEquals(TypeUtils.isCaseClass[SensitiveData[String]], true)
    assertEquals(TypeUtils.isCaseClass[(String, Int)], true) // surprise!
    assertEquals(TypeUtils.isCaseClass[EmptyTuple], true) // surprise!
    // opaque type without bounds
    assertEquals(TypeUtils.isCaseClass[Disability], false)
    // neg
    assertEquals(TypeUtils.isCaseClass[DriverLicense], false)
    assertEquals(TypeUtils.isCaseClass[Skills], false)
    assertEquals(TypeUtils.isCaseClass[Boats], false)
    assertEquals(TypeUtils.isCaseClass[Option[String]], false)
    assertEquals(TypeUtils.isCaseClass[List[String]], false)
    assertEquals(TypeUtils.isCaseClass[Array[String]], false)
    assertEquals(TypeUtils.isCaseClass[Hobby], false)
    assertEquals(TypeUtils.isCaseClass[PassportNumber], false)
    assertEquals(TypeUtils.isCaseClass[ImmigrationStatus], false)
    assertEquals(TypeUtils.isCaseClass[String], false)
    assertEquals(TypeUtils.isCaseClass[Int], false)
    assertEquals(TypeUtils.isCaseClass[Boolean], false)
    assertEquals(TypeUtils.isCaseClass[Double], false)
    assertEquals(TypeUtils.isCaseClass[Float], false)
    assertEquals(TypeUtils.isCaseClass[Long], false)
    assertEquals(TypeUtils.isCaseClass[Short], false)
    assertEquals(TypeUtils.isCaseClass[Byte], false)
    assertEquals(TypeUtils.isCaseClass[Char], false)
    assertEquals(TypeUtils.isCaseClass[Name], false)
    assertEquals(TypeUtils.isCaseClass[Name | false], false)
    assertEquals(TypeUtils.isCaseClass[NonEmptyTuple], false)
  }

  test("isString") {
    assertEquals(TypeUtils.isString[String], true)
    // neg
    assertEquals(TypeUtils.isString[PassportNumber], false)
    assertEquals(TypeUtils.isString[Int], false)
    assertEquals(TypeUtils.isString[Boolean], false)
    assertEquals(TypeUtils.isString[Double], false)
    assertEquals(TypeUtils.isString[Float], false)
    assertEquals(TypeUtils.isString[Long], false)
    assertEquals(TypeUtils.isString[Short], false)
    assertEquals(TypeUtils.isString[Byte], false)
    assertEquals(TypeUtils.isString[Disability], false)
    assertEquals(TypeUtils.isString[Hobby], false)
    assertEquals(TypeUtils.isString[Hobby.Other], false)
    assertEquals(TypeUtils.isString[Hobby.Cooking.type], false)
    assertEquals(TypeUtils.isString[SensitiveData[String]], false)
  }

  test("isOpaque") {
    assertEquals(TypeUtils.isOpaque[PassportNumber], true)
    assertEquals(TypeUtils.isOpaque[Disability], true)
    assertEquals(TypeUtils.isOpaque[DriverLicense], true)
    assertEquals(TypeUtils.isOpaque[Skills], true)
    assertEquals(TypeUtils.isOpaque[Boats], true)
    // neg
    assertEquals(TypeUtils.isOpaque[Hobby], false)
    assertEquals(TypeUtils.isOpaque[Hobby.Other], false)
    assertEquals(TypeUtils.isOpaque[Hobby.Cooking.type], false)
    assertEquals(TypeUtils.isOpaque[SensitiveData[String]], false)
    assertEquals(TypeUtils.isOpaque[Name], false)
  }

  test("isEnum") {
    assertEquals(TypeUtils.isEnum[TestEnum], true)
    assertEquals(TypeUtils.isEnum[Hobby], true)
    assertEquals(TypeUtils.isEnum[Hobby.Other], true)
    assertEquals(TypeUtils.isEnum[Hobby.Cooking.type], true)
    assertEquals(TypeUtils.isEnum[Cars], true)
    assertEquals(TypeUtils.isEnum[Cars.Other], true)
    // neg
    assertEquals(TypeUtils.isEnum[Planes], false)
    assertEquals(TypeUtils.isEnum[Airbus], false)
    assertEquals(TypeUtils.isEnum[Boeing], false)
    assertEquals(TypeUtils.isEnum[Boats], false)
    assertEquals(TypeUtils.isEnum[SensitiveData[String]], false)
    assertEquals(TypeUtils.isEnum[Disability], false)
    assertEquals(TypeUtils.isEnum[PassportNumber], false)
    assertEquals(TypeUtils.isEnum[ImmigrationStatus], false)
    assertEquals(TypeUtils.isEnum[Name], false)
    assertEquals(TypeUtils.isEnum[Option[String]], false)
    assertEquals(TypeUtils.isEnum[List[String]], false)
    assertEquals(TypeUtils.isEnum[Array[String]], false)
    assertEquals(TypeUtils.isEnum[Int], false)
    assertEquals(TypeUtils.isEnum[Boolean], false)
    assertEquals(TypeUtils.isEnum[Double], false)
    assertEquals(TypeUtils.isEnum[Float], false)
    assertEquals(TypeUtils.isEnum[Long], false)
    assertEquals(TypeUtils.isEnum[Short], false)
    assertEquals(TypeUtils.isEnum[Byte], false)
    assertEquals(TypeUtils.isEnum[Char], false)
    assertEquals(TypeUtils.isEnum[(String, Int)], false)
    assertEquals(TypeUtils.isEnum[NonEmptyTuple], false)
    assertEquals(TypeUtils.isEnum[EmptyTuple], false)
  }

  test("isUnion") {
    assertEquals(TypeUtils.isUnion[String | Int], true)
    assertEquals(TypeUtils.isUnion[Int | Boolean | "foo"], true)
    // neg
    assertEquals(TypeUtils.isUnion[SensitiveData[String]], false)
    assertEquals(TypeUtils.isUnion[Disability], false)
    assertEquals(TypeUtils.isUnion[PassportNumber], false)
    assertEquals(TypeUtils.isUnion[ImmigrationStatus], false)
    assertEquals(TypeUtils.isUnion[Name], false)
    assertEquals(TypeUtils.isUnion[Option[String]], false)
    assertEquals(TypeUtils.isUnion[List[String]], false)
    assertEquals(TypeUtils.isUnion[Array[String]], false)
    assertEquals(TypeUtils.isUnion[Int], false)
    assertEquals(TypeUtils.isUnion[Boolean], false)
    assertEquals(TypeUtils.isUnion[Double], false)
    assertEquals(TypeUtils.isUnion[Float], false)
    assertEquals(TypeUtils.isUnion[Long], false)
    assertEquals(TypeUtils.isUnion[Short], false)
    assertEquals(TypeUtils.isUnion[Byte], false)
    assertEquals(TypeUtils.isUnion[Char], false)
    assertEquals(TypeUtils.isUnion[String], false)
  }

  test("inspectUnion") {
    assertEquals(
      TypeUtils.inspectUnion[String | Int],
      List("java.lang.String", "scala.Int")
    )
    assertEquals(
      TypeUtils.inspectUnion[Int | false],
      List("scala.Int", "false")
    )
    assertEquals(
      TypeUtils.inspectUnion[Hobby | Boolean | "foo"],
      List(
        "org.encalmo.utils.Hobby",
        "scala.Boolean",
        "\"foo\""
      )
    )

    assertEquals(TypeUtils.inspectUnion[Option[String]], Nil)
    assertEquals(TypeUtils.inspectUnion[List[String]], Nil)
    assertEquals(TypeUtils.inspectUnion[Array[String]], Nil)
    assertEquals(TypeUtils.inspectUnion[Int], Nil)
  }

  test("isNonEmptyTuple") {
    assertEquals(TypeUtils.isNonEmptyTuple[(String, Int)], true)
    assertEquals(TypeUtils.isNonEmptyTuple[NonEmptyTuple], true)
    assertEquals(TypeUtils.isNonEmptyTuple[String *: EmptyTuple], true)
    assertEquals(TypeUtils.isNonEmptyTuple[String *: Int *: EmptyTuple], true)
    // neg
    assertEquals(TypeUtils.isNonEmptyTuple[EmptyTuple], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Option[String]], false)
    assertEquals(TypeUtils.isNonEmptyTuple[List[String]], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Array[String]], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Int], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Boolean], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Double], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Float], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Long], false)
    assertEquals(TypeUtils.isNonEmptyTuple[Short], false)
  }

  test("isTuple") {
    assertEquals(TypeUtils.isTuple[(String, Int)], true)
    assertEquals(TypeUtils.isTuple[NonEmptyTuple], true)
    assertEquals(TypeUtils.isTuple[String *: EmptyTuple], true)
    assertEquals(TypeUtils.isTuple[String *: Int *: EmptyTuple], true)
    assertEquals(TypeUtils.isTuple[EmptyTuple], true)
    // neg
    assertEquals(TypeUtils.isTuple[Option[String]], false)
    assertEquals(TypeUtils.isTuple[List[String]], false)
    assertEquals(TypeUtils.isTuple[Array[String]], false)
    assertEquals(TypeUtils.isTuple[Int], false)
    assertEquals(TypeUtils.isTuple[Boolean], false)
    assertEquals(TypeUtils.isTuple[Double], false)
    assertEquals(TypeUtils.isTuple[Float], false)
    assertEquals(TypeUtils.isTuple[Long], false)
    assertEquals(TypeUtils.isTuple[Short], false)
    assertEquals(TypeUtils.isTuple[Byte], false)
    assertEquals(TypeUtils.isTuple[Char], false)
  }

  test("isSealedADT") {
    assertEquals(TypeUtils.isSealedADT[Planes], true)
    assertEquals(TypeUtils.isSealedADT[Option[String]], true)
    assertEquals(TypeUtils.isSealedADT[List[String]], true)
    assertEquals(TypeUtils.isSealedADT[NonEmptyTuple], true)
    // neg
    assertEquals(TypeUtils.isSealedADT[Airbus], false)
    assertEquals(TypeUtils.isSealedADT[Boeing], false)
    assertEquals(TypeUtils.isSealedADT[Hobby], false)
    assertEquals(TypeUtils.isSealedADT[TestEnum], false)
    assertEquals(TypeUtils.isSealedADT[SensitiveData[String]], false)
    assertEquals(TypeUtils.isSealedADT[Disability], false)
    assertEquals(TypeUtils.isSealedADT[PassportNumber], false)
    assertEquals(TypeUtils.isSealedADT[ImmigrationStatus], false)
    assertEquals(TypeUtils.isSealedADT[Name], false)
    assertEquals(TypeUtils.isSealedADT[Array[String]], false)
    assertEquals(TypeUtils.isSealedADT[Int], false)
    assertEquals(TypeUtils.isSealedADT[Boolean], false)
    assertEquals(TypeUtils.isSealedADT[Double], false)
    assertEquals(TypeUtils.isSealedADT[Float], false)
    assertEquals(TypeUtils.isSealedADT[Long], false)
    assertEquals(TypeUtils.isSealedADT[Short], false)
    assertEquals(TypeUtils.isSealedADT[Byte], false)
    assertEquals(TypeUtils.isSealedADT[Char], false)
    assertEquals(TypeUtils.isSealedADT[(String, Int)], false)
    assertEquals(TypeUtils.isSealedADT[EmptyTuple], false)
  }

  test("isNamedTuple") {
    assertEquals(TypeUtils.isNamedTuple[(name: String, age: Int, email: String)], true)
    type Record = (name: String, age: Int, email: String)
    assertEquals(TypeUtils.isNamedTuple[Record], true)
    // neg
    assertEquals(TypeUtils.isNamedTuple[EmptyTuple], false)
    assertEquals(TypeUtils.isNamedTuple[NonEmptyTuple], false)
    assertEquals(TypeUtils.isNamedTuple[String *: EmptyTuple], false)
    assertEquals(TypeUtils.isNamedTuple[String *: Int *: EmptyTuple], false)
    assertEquals(TypeUtils.isNamedTuple[Option[String]], false)
    assertEquals(TypeUtils.isNamedTuple[List[String]], false)
    assertEquals(TypeUtils.isNamedTuple[Array[String]], false)
    assertEquals(TypeUtils.isNamedTuple[Int], false)
    assertEquals(TypeUtils.isNamedTuple[Boolean], false)
  }

  test("isJavaRecord") {
    assertEquals(TypeUtils.isJavaRecord[Order], true)
    // neg
    assertEquals(TypeUtils.isJavaRecord[SensitiveData[String]], false)
    assertEquals(TypeUtils.isJavaRecord[Disability], false)
    assertEquals(TypeUtils.isJavaRecord[PassportNumber], false)
    assertEquals(TypeUtils.isJavaRecord[ImmigrationStatus], false)
  }

}
