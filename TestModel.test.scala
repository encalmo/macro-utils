package org.encalmo.utils

import java.time.LocalDate

trait ImmigrationStatus {
  def status: String
  def validUntil: LocalDate
}

enum MaritalStatus {
  case Single
  case CivilPartnership(partnerName: LocalDate, from: LocalDate)
  case Married(partnerName: String, from: LocalDate)
  case Divorced(from: LocalDate)
  case Widowed(from: LocalDate)
}

enum Hobby {
  case Reading
  case Swimming
  case Cycling
  case Cooking
  case Other(name: String)
}

opaque type PassportNumber = String
object PassportNumber {
  def apply(value: String): PassportNumber = value
}

case class SensitiveData[T](value: T)

opaque type Disability = SensitiveData[String]
object Disability {
  def apply(value: String): Disability = SensitiveData(value)
}

opaque type DriverLicense <: Document = Document
object DriverLicense {
  def apply(number: String, expiryDate: LocalDate): DriverLicense = Document(number, expiryDate)
}

case class Document(
    number: String,
    expiryDate: LocalDate
)

opaque type Skills <: Set[String] = Set[String]
object Skills {
  def apply(values: String*): Skills = values.toSet
}

type Name = String

enum TestEnum {
  case Foo
  case Bar(name: String)
}

enum Benefit {
  case ChildBenefit
  case UniversalCredit
  case JobSeekersAllowance
  case EmploymentSupportAllowance
  case HousingBenefit
  case PensionCredit
  case Other(name: String)
}

enum Cars {
  case Ford
  case Toyota
  case Honda
  case Nissan
  case Other(name: String)
}

opaque type Boats <: Set[String] = Set[String]
object Boats {
  def apply(values: String*): Boats = values.toSet
}

sealed trait Planes
case class Boeing(model: String) extends Planes
case class Airbus(model: String) extends Planes

class Row extends Selectable {
  type Fields <: Any
}

class FactsRow extends Row {
  type Fields = (name: String, age: Int, email: String)

  transparent inline def selectDynamic(name: String): Any =
    name match {
      case "name"  => "John Doe"
      case "age"   => 30
      case "email" => "john.doe@example.com"
      case _       => throw new NoSuchElementException(s"Field $name not found")
    }
}
