package org.encalmo.utils

import scala.quoted.*

object TypeNameUtils {

  inline def shortBaseName(name: String): String = {
    val name2 = name.indexOf('[') match {
      case -1  => name
      case idx => name.substring(0, idx)
    }
    name2.lastIndexOf('.') match {
      case -1  => name2.replace("\"", "").replaceAll("\\s|\\|", "_")
      case idx => name2.substring(idx + 1)
    }
  }

  inline def underscored(name: String): String = {
    name.replaceAll("\\.|\\s|\\|", "_")
  }

  inline def toValueName(name: String): String = {
    val name2 = name.head.toLower +: name.tail.filter(c => java.lang.Character.isJavaIdentifierPart(c))
    if (KeywordUtils.needsBackticks(name2)) then s"`$name2`" else name2
  }

  transparent inline def typeName[A] = ${ typeNameExpr[A] }
  def typeNameExpr[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprShortCode)
    Expr(shortBaseName(name))

  def typeNameOf[A: Type](using Quotes): String =
    import quotes.reflect.*
    val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprShortCode)
    shortBaseName(name)

  inline def typeNames[A]: Tuple = ${ typeNamesExpr[A] }
  def typeNamesExpr[A: Type](using Quotes): Expr[Tuple] =
    Type.of[A] match
      case '[elem *: elems] => '{ typeName[elem] *: typeNames[elems] }
      case _                => '{ EmptyTuple }

  def typeNamesOf[A <: Tuple: Type](using Quotes): Tuple =
    Type.of[A] match
      case '[elem *: elems] => typeNameOf[elem] *: typeNamesOf[elems]
      case _                => EmptyTuple

  transparent inline def valueName[A] = ${ valueNameExpr[A] }
  def valueNameExpr[A: Type](using Quotes): Expr[String] =
    import quotes.reflect.*
    val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprShortCode)
    Expr(toValueName(name))

  def valueNameOf[A: Type](using Quotes): String =
    import quotes.reflect.*
    val name = TypeRepr.of[A].dealias.show(using Printer.TypeReprShortCode)
    toValueName(name)

}

object KeywordUtils {

  // Always reserved keywords
  private val hardKeywords: Set[String] = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  // Contextual keywords (often safe to use as vars, but best avoided)
  private val softKeywords: Set[String] = Set(
    "as",
    "derives",
    "end",
    "extension",
    "infix",
    "inline",
    "opaque",
    "open",
    "transparent",
    "using"
  )

  def isScalaKeyword(s: String): Boolean = hardKeywords.contains(s)

  def isSoftKeyword(s: String): Boolean = softKeywords.contains(s)

  /** Returns true if the string must be wrapped in backticks (e.g., `type`) to be used as an identifier.
    */
  def needsBackticks(s: String): Boolean = {
    isScalaKeyword(s) ||
    s.isEmpty ||
    !s.head.isUnicodeIdentifierStart ||
    !s.tail.forall(_.isUnicodeIdentifierPart)
  }
}
