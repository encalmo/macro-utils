<a href="https://github.com/encalmo/macro-utils">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/macro-utils_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/macro-utils_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/macro-utils/scaladoc/org/encalmo/utils.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# macro-utils

Macro-utils is a Scala 3 library providing some helpers for building macros. Uses visitor and transform patterns to explore types.

### Provided Utility Objects

- `CaseClassUtils` visit and transfrom case class fields, names and values
- `EnumUtils` visit enums and sealed ADT hierarchies, and Java's enums, and build case pattern matcher
- `UnionUtils` visit types of the union
- `TupleUtils` visit tuple and named tuple elements
- `OpaqueTypeUtils` unwrap the type is possible
- `SelectableUtils` maybe visit selectable fields
- `JavaRecordUtils` visit record fields, names and values
- `JavaMapUtils` visit Java's Map names and values
- `JavaIterableUtils` visit Java' iterables

## Table of contents

   - [Provided Utility Objects](#provided-utility-objects)
- [Dependencies](#dependencies)
- [Usage](#usage)
- [Examples](#examples)
   - [Example: Using `CaseClassUtils.visit` to Inspect Case Class Fields](#example:-using-`caseclassutils.visit`-to-inspect-case-class-fields)
   - [Example: Using `CaseClassUtils.transformToList` to create an instance with all string values upper cased](#example:-using-`caseclassutils.transformtolist`-to-create-an-instance-with-all-string-values-upper-cased)
   - [Example: Using `CaseClassUtils.transformToExprOfTuple` to create a tuple from case class fields](#example:-using-`caseclassutils.transformtoexproftuple`-to-create-a-tuple-from-case-class-fields)
- [Project content](#project-content)

## Dependencies

   - [Scala](https://www.scala-lang.org) >= 3.7.4

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" %% "macro-utils" % "0.9.2"

or with SCALA-CLI

    //> using dep org.encalmo::macro-utils:0.9.2

## Examples

### Example: Using `CaseClassUtils.visit` to Inspect Case Class Fields

Suppose you want to write a macro that visits all fields of a case class and prints their names and values at compile time. The `CaseClassUtils.visit` method can be used to achieve this in a concise way.

```scala
import org.encalmo.utils.CaseClassUtils
import scala.quoted.*

inline def printCaseClassFields[T](inline value: T): Unit = ${ printCaseClassFieldsImpl('value) }

  def printCaseClassFieldsImpl[T: Type](value: Expr[T])(using Quotes): Expr[Unit] = {
    CaseClassUtils.visit[T](
      value,
      [A: Type] => (nameExpr, valueExpr, annotations) =>
          '{ println(${ nameExpr } + ": " + ${ valueExpr }.toString) }
    )
  }
```

**Usage:**

```scala
case class Person(name: String, age: Int)
printCaseClassFields(Person("Alice", 30))
```

The output will be:
```
name: Alice
age: 30
```

This demonstrates how to traverse the fields of a case class using `CaseClassUtils.visit` and apply a custom function to each field.

### Example: Using `CaseClassUtils.transformToList` to create an instance with all string values upper cased

This shows how to write a macro using `CaseClassUtils.transformToList` that upper-cases all `String` fields, while leaving other field types unchanged.

```scala
import org.encalmo.utils.CaseClassUtils
import org.encalmo.utils.AnnotationUtils.AnnotationInfo
import scala.quoted.*

inline def upperCaseStringFields[T, R <: Product](inline value: T): R =
    ${ upperCaseStringFieldsImpl[T, R]('value) }

def upperCaseStringFieldsImpl[T: Type, R <: Product: Type](value: Expr[T])(using Quotes): Expr[R] = {
  import quotes.reflect.*
  val args = CaseClassUtils.transformToList[T, quotes.reflect.Term](
    value,
    [A: Type] =>
      (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
        Some {
          Type.of[A] match {
            case '[String] =>
              '{ ${ valueExpr.asExprOf[String] }.toUpperCase }.asTerm
            case _ =>
              valueExpr.asTerm
          }
        }
  )

  CaseClassUtils.createInstanceUsingConstructor[R](args)
}
```

### Example: Using `CaseClassUtils.transformToExprOfTuple` to create a tuple from case class fields

Sometimes you might want to produce a tuple of fields or computed values from a case class at compile-time. Here's how you can use `CaseClassUtils.transformToExprOfTuple` in a macro to do this.

```scala
import org.encalmo.utils.CaseClassUtils
import org.encalmo.utils.AnnotationUtils.AnnotationInfo
import scala.quoted.*

inline def extractFieldTuple[T](inline value: T): Tuple =
  ${ extractFieldTupleImpl[T]('value) }

def extractFieldTupleImpl[T: Type](value: Expr[T])(using Quotes): Expr[Tuple] = {
  CaseClassUtils.transformToExprOfTuple[T](
    value,
    [A: Type] =>
      (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
        // For demonstration, we just put the value itself into the tuple
        Some(valueExpr)
  )
}
```

**Usage:**

```scala
case class Foo(a: String, b: Int)
val foo = Foo("bar", 42)
val tup = extractFieldTuple(foo)
assert(tup == ("bar", 42))
```

This will produce a tuple containing all the field values in order: `("bar", 42)` for the example above.


**Usage:**

```scala
case class User(name: String, email: String, age: Int)
val user = User("alice", "alice@example.com", 30)
assertEquals(
    upperCaseStringFields(user), 
     User("ALICE", "ALICE@EXAMPLE.COM", 30)
)
```



## Project content

```
├── .github
│   └── workflows
│       ├── pages.yaml
│       ├── release.yaml
│       └── test.yaml
│
├── .gitignore
├── .scalafmt.conf
├── AnnotationUtils.scala
├── CaseClassUtils.scala
├── CaseClassUtils.test.scala
├── CaseClassUtilsTestMacro.test.scala
├── DebugUtils.scala
├── EnumUtils.scala
├── EnumUtils.test.scala
├── EnumUtilsTestMacro.test.scala
├── JavaIterableUtils.scala
├── JavaMapUtils.scala
├── JavaRecordUtils.scala
├── JavaRecordUtils.test.scala
├── JavaRecordUtilsTestMacro.test.scala
├── LICENSE
├── MethodsCache.scala
├── MethodUtils.scala
├── MethodUtils.test.scala
├── MethodUtilsTestMacro.test.scala
├── OpaqueTypeUtils.scala
├── OpaqueTypeUtils.test.scala
├── OpaqueUtilsTestMacro.test.scala
├── Order.java
├── project.scala
├── README.md
├── SelectableUtils.scala
├── Status.java
├── test.sh
├── TestModel.test.scala
├── TupleUtils.scala
├── TupleUtils.test.scala
├── TupleUtilsTestMacro.test.scala
├── TypeNameUtils.scala
├── TypeNameUtils.test.scala
├── TypeUtils.scala
├── TypeUtils.test.scala
├── UnionUtils.scala
├── UnionUtils.test.scala
└── UnionUtilsTestMacro.test.scala
```

