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

  test("select a term from a case class") {
    assertEquals(
      testMaybeSelectTerm("name", human),
      "MethodUtilsSpec.this.human.name: String = John"
    )
  }

  test("select a term from a case class") {
    assertEquals(
      testMaybeSelectTerm("age", human),
      "MethodUtilsSpec.this.human.age: Int = 30"
    )
  }

  test("select getName term from a class") {
    assertEquals(
      testMaybeSelectTerm("foo", animal),
      "MethodUtilsSpec.this.animal.foo: String = Rex5"
    )
  }

  test("select getName term from a class") {
    assertEquals(
      testMaybeSelectTerm("name", animal),
      "MethodUtilsSpec.this.animal.name: String = REX"
    )
  }

  test("select getAge term from a class") {
    assertEquals(
      testMaybeSelectTerm("age", animal),
      "MethodUtilsSpec.this.animal.age: Int = 5"
    )
  }

  test("select a value from a case class") {
    assertEquals(
      testMaybeSelectedValue("name", human),
      "MethodUtilsSpec.this.human.name: String = John"
    )
  }

  test("select a value from a case class") {
    assertEquals(
      testMaybeSelectedValue("age", human),
      "MethodUtilsSpec.this.human.age: Int = 30"
    )
  }

  test("select a method from a class") {
    assertEquals(
      testMaybeSelectedValue("foo", animal),
      "MethodUtilsSpec.this.animal.foo: String = Rex5"
    )
  }

  test("select getName method from a class") {
    assertEquals(
      testMaybeSelectedValue("name", animal),
      "MethodUtilsSpec.this.animal.getName(): String = REX"
    )
  }

  test("select getAge method from a class") {
    assertEquals(
      testMaybeSelectedValue("age", animal),
      "MethodUtilsSpec.this.animal.getAge(): Int = 5"
    )
  }

  test("call or build method of unit with cache") {
    testCallOrBuildMethodOfUnitWithCache("foo")
  }

  test("call a method with multiple arguments on a class") {
    assertEquals(
      testMethodCall(new Testy("bar")),
      "bar:foo1-2"
    )
  }

}
