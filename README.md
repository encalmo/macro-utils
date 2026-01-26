<a href="https://github.com/encalmo/macro-utils">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/macro-utils_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/macro-utils_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/macro-utils/scaladoc/org/encalmo/utils.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# macro-utils

Macro-utils is a Scala 3 library providing some helpers for building macros. Uses visitor and transform patterns to explore types.

### Provided Utility Objects

- `TypeNameUtils`
- `TypeUtils`
- `CaseClassUtils` visit and transfrom case class fields, names and values
- `EnumUtils` visit enums and sealed ADT hierarchies, and Java's enums, and build case pattern matcher
- `UnionUtils` visit types of the union
- `TupleUtils` visit tuple and named tuple elements
- `OpaqueTypeUtils` unwrap the type is possible
- `JavaRecordUtils` visit record fields, names and values
- `JavaMapUtils` visit Java's Map names and values
- `JavaIterableUtils` visit Java' iterables

## Dependencies

   - [Scala](https://www.scala-lang.org) >= 3.7.4

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" %% "macro-utils" % "0.9.0"

or with SCALA-CLI

    //> using dep org.encalmo::macro-utils:0.9.0