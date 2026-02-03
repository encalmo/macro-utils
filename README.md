<a href="https://github.com/encalmo/macro-utils">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/macro-utils_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/macro-utils_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/macro-utils/scaladoc/org/encalmo/utils.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# macro-utils

`macro-utils` is a Scala 3 library providing helpers for building macros. Leverages visitor pattern to traverse data structures of different types. All major methods have a variant using `StatementsCache` abstraction to avoid nested splicing conflicts and cache/reuse produced methods.

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
   - [Example: Using `CaseClassUtils.visit` to Perform an Operation on Case Class Fields](#example:-using-`caseclassutils.visit`-to-perform-an-operation-on-case-class-fields)
   - [Example: Using `CaseClassUtils.collect` to Inspect Case Class Fields](#example:-using-`caseclassutils.collect`-to-inspect-case-class-fields)
   - [Example: Using `CaseClassUtils.transformToList` to create an instance with all string values upper cased](#example:-using-`caseclassutils.transformtolist`-to-create-an-instance-with-all-string-values-upper-cased)
   - [Example: Using `CaseClassUtils.transformToExprOfTuple` to create a tuple from case class fields](#example:-using-`caseclassutils.transformtoexproftuple`-to-create-a-tuple-from-case-class-fields)
- [Project content](#project-content)

## Dependencies

   - [Scala](https://www.scala-lang.org) >= 3.7.4

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" %% "macro-utils" % "0.9.8"

or with SCALA-CLI

    //> using dep org.encalmo::macro-utils:0.9.8

## Examples

### Example: Using `CaseClassUtils.visit` to Perform an Operation on Case Class Fields

You can use `CaseClassUtils.visit` when you want to operate on each field of a case class, such as accumulating side effects or constructing values using a cache. The following macro visits each field of a case class and prints its name and value at compile time, leveraging the `StatementsCache` to build block of statements.

```scala
import org.encalmo.utils.CaseClassUtils
import org.encalmo.utils.StatementsCache
import scala.quoted.*

inline def printCaseClassFields[T](inline value: T): Unit = ${ printCaseClassFieldsImpl('value) }

def printCaseClassFieldsImpl[T: Type](value: Expr[T])(using Quotes): Expr[Unit] = {
  given StatementsCache = new StatementsCache
  printCaseClassFieldsUsingCache[T](value)
}

def printCaseClassFieldsUsingCache[T: Type](valueExpr: Expr[T])(using cache: StatementsCache): Expr[Unit] = {
  given cache.quotes.type = cache.quotes
  import cache.quotes.reflect.*

  CaseClassUtils.visit[T](using cache)(
    valueExpr.asTerm,
    [A: Type] =>
      (name, value, annotations) =>
        val term = MethodUtils.callPrintln(using cache.quotes)(
          Literal(StringConstant(name)),
          Literal(StringConstant(": ")),
          StringUtils.applyToString(value)
        )
        cache.addStatement(term)
  )
  cache.getBlockExprOfUnit
}
```

**Usage:**

```scala
case class Point(x: Int, y: Int)
printCaseClassFields(Point(10, 20))
```

Output:
```
x: 10
y: 20
```

This demonstrates how to walk through a case class with `CaseClassUtils.visit` and perform a custom action for each field, using the macro utilities' cache for correct code generation.


### Example: Using `CaseClassUtils.collect` to Inspect Case Class Fields

Suppose you want to write a macro that visits all fields of a case class and prints their names and values at compile time. The `CaseClassUtils.collect` method can be used to achieve this in a concise way.

```scala
import org.encalmo.utils.CaseClassUtils
import scala.quoted.*

inline def printCaseClassFields[T](inline value: T): Unit = ${ printCaseClassFieldsImpl('value) }

  def printCaseClassFieldsImpl[T: Type](value: Expr[T])(using Quotes): Expr[Unit] = {
    CaseClassUtils.collect[T](
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
  val args: List[quotes.reflect.Term] = 
    CaseClassUtils.transformToList[T, quotes.reflect.Term](
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

**Usage:**

```scala
 case class User(name: String, email: String, age: Int)
  val user = User("alice", "alice@example.com", 30)
  assert(upperCaseStringFields[User, User](user) == User("ALICE", "ALICE@EXAMPLE.COM", 30))
```

### Example: Using `CaseClassUtils.transformToExprOfTuple` to create a tuple from case class fields

Sometimes you might want to produce a tuple of fields or computed values from a case class at compile-time. Here's how you can use `CaseClassUtils.transformToExprOfTuple` in a macro to do this.

```scala
import org.encalmo.utils.CaseClassUtils
import org.encalmo.utils.AnnotationUtils.AnnotationInfo
import scala.quoted.*

inline def upperCaseStringFields2[T, R <: Product](inline value: T): R =
  ${ upperCaseStringFields2Impl[T, R]('value) }

def upperCaseStringFields2Impl[T: Type, R <: Product: Type](value: Expr[T])(using Quotes): Expr[R] = {
   val tuple: Expr[Tuple] = CaseClassUtils.transformToExprOfTuple[T](
    value,
    [A: Type] =>
      (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
        Some {
          Type.of[A] match
            case '[String] =>
              '{ ${ valueExpr.asExprOf[String] }.toUpperCase }
            case _ =>
              valueExpr
        }
  )
  CaseClassUtils.createInstanceFromTuple[R](tuple)
}
```

**Usage:**

```scala
 case class User(name: String, email: String, age: Int)
  val user = User("alice", "alice@example.com", 30)
  assert(upperCaseStringFields2[User, User](user) == User("ALICE", "ALICE@EXAMPLE.COM", 30))
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
├── MethodUtils.scala
├── MethodUtils.test.scala
├── MethodUtilsTestMacro.test.scala
├── OpaqueTypeUtils.scala
├── OpaqueTypeUtils.test.scala
├── OpaqueUtilsTestMacro.test.scala
├── Order.java
├── project.scala
├── QuotesUtils.scala
├── README.md
├── SelectableUtils.scala
├── SelectableUtils.test.scala
├── SelectableUtilsTestMacro.test.scala
├── StatementsCache.scala
├── Status.java
├── StringUtils.scala
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

