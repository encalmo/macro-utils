package org.encalmo.utils

import MethodUtilsTestMacro.*

class MethodUtilsSpec extends munit.FunSuite {

  case class Human(name: String, age: Int)
  val human = Human("John", 30)

  class Animal(name: String, age: Int) {
    def foo: String = name + age
    def getName(): String = name.toUpperCase
    def getAge(): Int = age
  }
  val animal = new Animal("Rex", 5)

  test("select a value from a case class") {
    assertEquals(
      testMaybeSelectedValue("name", human),
      "name: String = John"
    )
  }

  test("select a value from a case class") {
    assertEquals(
      testMaybeSelectedValue("age", human),
      "age: Int = 30"
    )
  }

  test("select a method from a class") {
    assertEquals(
      testMaybeSelectedValue("foo", animal),
      "foo: String = Rex5"
    )
  }

  test("select getName method from a class") {
    assertEquals(
      testMaybeSelectedValue("name", animal),
      "name: String = REX"
    )
  }

  test("select getAge method from a class") {
    assertEquals(
      testMaybeSelectedValue("age", animal),
      "age: Int = 5"
    )
  }

  test("wrap in a method call") {
    val buffer = collection.mutable.ListBuffer.empty[String]
    testWrapInMethodCallWithCache(4, "foo", { buffer.append("foo") })
    assertEquals(buffer.size, 4)
  }

}
