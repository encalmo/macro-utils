package org.encalmo.utils

import UnionUtilsTestMacro.*

class UnionUtilsSpec extends munit.FunSuite {

  type Entity = String | Int | Boolean

  val entity1: Entity = "Rex"
  val entity2: Entity = 1
  val entity3: Entity = true

  test("visit union's cases 1") {
    assertEquals(
      testVisitMethod(entity1),
      List(
        "case _: String =>",
        "case _: Int =>",
        "case _: Boolean =>"
      )
    )
  }

  test("visit union's cases 2") {
    assertEquals(
      testVisitMethod(entity2),
      List(
        "case _: String =>",
        "case _: Int =>",
        "case _: Boolean =>"
      )
    )
  }

  test("visit union's cases 3") {
    assertEquals(
      testVisitMethod(entity3),
      List(
        "case _: String =>",
        "case _: Int =>",
        "case _: Boolean =>"
      )
    )
  }

  test("visit non-union type") {
    assertEquals(
      testVisitMethod("not a union type"),
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

}
