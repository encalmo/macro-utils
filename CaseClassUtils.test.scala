package org.encalmo.utils

import CaseClassUtilsTestMacro.*

class CaseClassUtilsSpec extends munit.FunSuite {

  case class foo(name: String) extends scala.annotation.StaticAnnotation

  case class Entity(name: String, @foo("bar") age: Int, emails: List[String])
  val entity = Entity("John Doe", 30, List("john.doe@example.com", "jane.doe@example.com"))

  test("visit a case class") {
    assertEquals(
      testVisitMethod(entity),
      List(
        "name: String = John Doe",
        "@foo(name=bar)age: Int = 30",
        "emails: List[String] = List(john.doe@example.com, jane.doe@example.com)"
      )
    )
  }

  test("transform a case class") {
    assertEquals(
      testTransformMethod(entity),
      List(
        "name: String = John Doe",
        "age: Int = 30",
        "emails: List[String] = List(john.doe@example.com, jane.doe@example.com)"
      )
    )
  }

  test("visit not a case class") {
    assertEquals(
      testVisitMethod("not a case class"),
      Nil
    )
  }

  test("isCaseClass") {
    assertEquals(testIsCaseClass[Entity], true)
    assertEquals(testIsCaseClass[Hobby.Other], true)
    assertEquals(testIsCaseClass[SensitiveData[String]], true)
    assertEquals(testIsCaseClass[(String, Int)], true) // surprise!
    assertEquals(testIsCaseClass[EmptyTuple], true) // surprise!
    // opaque type without bounds
    assertEquals(testIsCaseClass[Disability], false)
    // neg
    assertEquals(testIsCaseClass[DriverLicense], false)
    assertEquals(testIsCaseClass[Skills], false)
    assertEquals(testIsCaseClass[Boats], false)
    assertEquals(testIsCaseClass[Option[String]], false)
    assertEquals(testIsCaseClass[List[String]], false)
    assertEquals(testIsCaseClass[Array[String]], false)
    assertEquals(testIsCaseClass[Hobby], false)
    assertEquals(testIsCaseClass[PassportNumber], false)
    assertEquals(testIsCaseClass[ImmigrationStatus], false)
    assertEquals(testIsCaseClass[String], false)
    assertEquals(testIsCaseClass[Int], false)
    assertEquals(testIsCaseClass[Boolean], false)
    assertEquals(testIsCaseClass[Double], false)
    assertEquals(testIsCaseClass[Float], false)
    assertEquals(testIsCaseClass[Long], false)
    assertEquals(testIsCaseClass[Short], false)
    assertEquals(testIsCaseClass[Byte], false)
    assertEquals(testIsCaseClass[Char], false)
    assertEquals(testIsCaseClass[Name], false)
    assertEquals(testIsCaseClass[Name | false], false)
    assertEquals(testIsCaseClass[NonEmptyTuple], false)
  }

  test("print case class fields") {
    case class Person(name: String, age: Int)
    printCaseClassFields(Person("Alice", 30))
  }

  test("print case class fields using statements cache") {
    case class Person(name: String, age: Int)
    printCaseClassFields2(Person("Alice", 30))
  }

  test("upper case string fields") {
    case class User(name: String, email: String, age: Int)
    val user = User("alice", "alice@example.com", 30)
    assertEquals(
      upperCaseStringFields[User, User](user),
      User("ALICE", "ALICE@EXAMPLE.COM", 30)
    )
  }

  test("upper case string fields 2") {
    case class User(name: String, email: String, age: Int)
    val user = User("alice", "alice@example.com", 30)
    assertEquals(upperCaseStringFields2[User, User](user), User("ALICE", "ALICE@EXAMPLE.COM", 30))
  }

  test("extract field tuple") {
    case class User(name: String, email: String, age: Int)
    val user = User("alice", "alice@example.com", 30)
    assertEquals(extractFieldTuple[User](user), ("alice", "alice@example.com", 30))
  }
}
