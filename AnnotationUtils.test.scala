package org.encalmo.utils

import AnnotationUtilsTestMacro.*

class AnnotationUtilsSpec extends munit.FunSuite {

  case class Foo(value: String) extends scala.annotation.StaticAnnotation

  case class Entity(
      @Foo("Bar") name: String,
      @Foo("42") age: Int,
      emails: List[String]
  )

  test("compute field annotations") {
    assertEquals(
      testComputeFieldAnnotations[Entity](
        Entity("John Doe", 30, List("john.doe@example.com", "jane.doe@example.com")),
        "name"
      ),
      List("@Foo(value=Bar)")
    )
    assertEquals(
      testComputeFieldAnnotations[Entity](
        Entity("John Doe", 30, List("john.doe@example.com", "jane.doe@example.com")),
        "age"
      ),
      List("@Foo(value=42)")
    )
  }

}
