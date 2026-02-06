<a href="https://github.com/encalmo/macro-utils">![GitHub](https://img.shields.io/badge/github-%23121011.svg?style=for-the-badge&logo=github&logoColor=white)</a> <a href="https://central.sonatype.com/artifact/org.encalmo/macro-utils_3" target="_blank">![Maven Central Version](https://img.shields.io/maven-central/v/org.encalmo/macro-utils_3?style=for-the-badge)</a> <a href="https://encalmo.github.io/macro-utils/scaladoc/org/encalmo/utils.html" target="_blank"><img alt="Scaladoc" src="https://img.shields.io/badge/docs-scaladoc-red?style=for-the-badge"></a>

# macro-utils

`macro-utils` is a Scala 3 library providing helpers for building macros. Leverages visitor pattern to traverse data structures of different types. All major methods have a variant using `StatementsCache` abstraction to avoid nested splicing conflicts and cache/reuse produced methods.

## Table of contents

- [Dependencies](#dependencies)
- [Usage](#usage)
- [Patterns](#patterns)
   - [StatementsCache](#statementscache)
- [Project content](#project-content)

## Dependencies

   - [Scala](https://www.scala-lang.org) >= 3.7.4

## Usage

Use with SBT

    libraryDependencies += "org.encalmo" %% "macro-utils" % "0.9.38"

or with SCALA-CLI

    //> using dep org.encalmo::macro-utils:0.9.38

## Patterns

### StatementsCache
StatementsCache is an internal utility class designed to manage and cache statements and symbols generated during macro execution. 

When writing macros in Scala 3 using quotes and nested splicing (`${...}`) frequently leads to conflicts and duplicated code. To mitigate those issues each instance of `StatementsCache` provides:

- append-only list of statements convertible at the end to an expression and returned from a macro
- helpers to create reusable methods and values in different scopes (local, outer, toplevel)
- method to fork off a nested cache preserving hierarchical reference lookup.

All utils in `macro-utils` accept a `StatementsCache` instance. This enables macro authors to compose complex, nested code-generation logic confidently while maintaining correctness and reusability.


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
├── AnnotationUtils.test.scala
├── AnnotationUtilsTestMacro.test.scala
├── ArrayUtils.scala
├── ArrayUtils.test.scala
├── ArrayUtilsTestMacro.test.scala
├── CaseClassUtils.scala
├── CaseClassUtils.test.scala
├── CaseClassUtilsTestMacro.test.scala
├── DebugUtils.scala
├── EitherUtils.scala
├── EitherUtils.test.scala
├── EitherUtilsTestMacro.test.scala
├── EnumUtils.scala
├── EnumUtils.test.scala
├── EnumUtilsTestMacro.test.scala
├── IterableUtils.scala
├── IterableUtils.test.scala
├── IterableUtilsTestMacro.test.scala
├── JavaIterable.test.scala
├── JavaIterableTestMacro.test.scala
├── JavaIterableUtils.scala
├── JavaMapUtils.scala
├── JavaMapUtilsSpec.test.scala
├── JavaMapUtilsTestMacro.test.scala
├── JavaRecordUtils.scala
├── JavaRecordUtils.test.scala
├── JavaRecordUtilsTestMacro.test.scala
├── LICENSE
├── MapUtils.scala
├── MapUtilsSpec.test.scala
├── MapUtilsTestMacro.test.scala
├── MethodUtils.scala
├── MethodUtils.test.scala
├── MethodUtilsTestMacro.test.scala
├── OpaqueTypeUtils.scala
├── OpaqueTypeUtils.test.scala
├── OpaqueUtilsTestMacro.test.scala
├── OptionUtils.scala
├── OptionUtils.test.scala
├── OptionUtilsTestMacro.test.scala
├── Order.java
├── project.scala
├── README.md
├── SelectableUtils.scala
├── SelectableUtils.test.scala
├── SelectableUtilsTestMacro.test.scala
├── StatementsCache.scala
├── StatementsCacheSpec.test.scala
├── StatementsCacheTestMacro.scala
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
├── TypeUtilsTestMacro.scala
├── UnionUtils.scala
├── UnionUtils.test.scala
└── UnionUtilsTestMacro.test.scala
```

