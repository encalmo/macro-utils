package org.encalmo.utils

import OptionUtils.*
import scala.quoted.*

object OptionUtilsTestMacro {

  inline def testBuildMatchTerm[A](value: Option[A]): String = {
    ${ testBuildMatchTermImpl[A]('{ value }) }
  }

  def testBuildMatchTermImpl[A: Type](valueExpr: Expr[Option[A]])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testBuildMatchTerm2Impl[A](valueExpr)
  }

  def testBuildMatchTerm2Impl[A: Type](using cache: StatementsCache)(valueExpr: Expr[Option[A]]): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    cache.put {
      buildMatchTerm[A](
        valueExpr.asTerm,
        onSome = { [B: Type] => term => StringUtils.applyToString(term) },
        onNone = { Literal(StringConstant("<none>")) }
      )
    }
    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }
}
