package org.encalmo.utils

import AnnotationUtils.*
import scala.quoted.*

object AnnotationUtilsTestMacro {

  inline def testComputeFieldAnnotations[A](entity: A, inline fieldName: String): List[String] = {
    ${ testComputeFieldAnnotationsImpl[A]('{ entity }, '{ fieldName }) }
  }

  def testComputeFieldAnnotationsImpl[A: Type](entityExpr: Expr[A], fieldNameExpr: Expr[String])(using
      Quotes
  ): Expr[List[String]] = {
    given StatementsCache = new StatementsCache
    testComputeFieldAnnotations2Impl[A](entityExpr, fieldNameExpr.valueOrAbort)
  }

  def testComputeFieldAnnotations2Impl[A: Type](using
      cache: StatementsCache
  )(
      entityExpr: Expr[A],
      fieldName: String
  ): Expr[List[String]] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    Expr.ofList(
      TypeRepr
        .of[A]
        .dealias
        .typeSymbol
        .caseFields
        .find(_.name == fieldName)
        .map(fields => computeFieldAnnotations[A](fieldName).map(_.toString))
        .getOrElse(Set.empty[String])
        .map(Expr(_))
        .toList
    )

  }
}
