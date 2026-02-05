package org.encalmo.utils

import UnionUtilsTestMacro.*

class UnionUtilsSpec extends munit.FunSuite {

  type Entity = String | Int | Boolean

  val entity1: Entity = "Rex"
  val entity2: Entity = 1
  val entity3: Entity = true

  test("transformToMatchExpression union's cases 1") {
    assertEquals(
      testTransformToMatchExpressionMethod(entity1),
      List(
        "case _: String =>",
        "case _: Int =>",
        "case _: Boolean =>"
      )
    )
  }

  test("transformToMatchExpression union's cases 2") {
    assertEquals(
      testTransformToMatchExpressionMethod(entity2),
      List(
        "case _: String =>",
        "case _: Int =>",
        "case _: Boolean =>"
      )
    )
  }

  test("transformToMatchExpression union's cases 3") {
    assertEquals(
      testTransformToMatchExpressionMethod(entity3),
      List(
        "case _: String =>",
        "case _: Int =>",
        "case _: Boolean =>"
      )
    )
  }

  test("transformToMatchExpression non-union type") {
    assertEquals(
      testTransformToMatchExpressionMethod("not a union type"),
      Nil
    )
  }

  test("isUnion") {
    assertEquals(testIsUnion[Entity], true)
    assertEquals(testIsUnion[String | Boolean], true)
    // neg
    assertEquals(testIsUnion[Option[String]], false)
    assertEquals(testIsUnion[List[String]], false)
    assertEquals(testIsUnion[Array[String]], false)
    assertEquals(testIsUnion[Int], false)
    assertEquals(testIsUnion[Boolean], false)
    assertEquals(testIsUnion[Double], false)
    assertEquals(testIsUnion[Float], false)
    assertEquals(testIsUnion[Long], false)
    assertEquals(testIsUnion[Short], false)
    assertEquals(testIsUnion[Byte], false)
    assertEquals(testIsUnion[Char], false)
    assertEquals(testIsUnion[(String, Int)], false)
    assertEquals(testIsUnion[EmptyTuple], false)
  }

  test("transformToMatchTerm union's cases 1") {
    assertEquals(
      testTransformToMatchTermMethod(entity1),
      "case _: String => Rex"
    )
    assertEquals(
      testTransformToMatchTermMethod(entity2),
      "case _: Int => 1"
    )
    assertEquals(
      testTransformToMatchTermMethod(entity3),
      "case _: Boolean => true"
    )
    assertEquals(
      testTransformToMatchTermMethod("not a union type"),
      "not an union type"
    )
  }

  test("transformTupleToMatchTerm union's cases 1") {
    type Union = String | Int | Boolean
    val e1: Union = "Rex"
    val e2: Union = 1
    val e3: Union = true
    val entity: (Union, Union, Union) = (e1, e2, e3)
    assertEquals(
      testTransformTupleToMatchTermMethod(entity),
      "case _: String => Rex, case _: Int => 1, case _: Boolean => true"
    )
  }

  test("transformTupleToMatchTerm union's cases 2") {
    type Union = String | Int | Boolean
    val e1: Union = "Rex"
    val e2: Union = 1
    val e3: Union = true
    val entity: (a: Union, b: Union, c: Union) = (a = e1, b = e2, c = e3)
    assertEquals(
      testTransformTupleToMatchTermMethod(entity),
      "case _: String => Rex, case _: Int => 1, case _: Boolean => true"
    )
  }

}
