package org.encalmo.utils

import EitherUtils.*
import scala.quoted.*

object EitherUtilsTestMacro {

  inline def testBuildMatchTerm[L, R](value: Either[L, R]): String = {
    ${ testBuildMatchTermImpl[L, R]('{ value }) }
  }

  def testBuildMatchTermImpl[L: Type, R: Type](valueExpr: Expr[Either[L, R]])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testBuildMatchTerm2Impl[L, R](valueExpr)
  }

  def testBuildMatchTerm2Impl[L: Type, R: Type](using
      cache: StatementsCache
  )(valueExpr: Expr[Either[L, R]]): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    cache.addStatement {
      buildMatchTerm[L, R](
        valueExpr.asTerm,
        onLeft = { [B: Type] => term =>
          StringUtils
            .concat(
              Literal(StringConstant("Left(")),
              StringUtils.applyToString(term),
              Literal(StringConstant(")"))
            )
        },
        onRight = { [C: Type] => term =>
          StringUtils
            .concat(
              Literal(StringConstant("Right(")),
              StringUtils.applyToString(term),
              Literal(StringConstant(")"))
            )
        }
      )
    }
    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }
}
