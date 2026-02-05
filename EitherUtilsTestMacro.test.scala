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

    TypeRepr.of[Either[L, R]] match {
      case TypeReprIsEither(leftTpe, rightTpe) =>
        cache.put {
          buildMatchTerm(
            leftTpe,
            rightTpe,
            valueExpr.asTerm,
            functionOnLeft = { (tpe, term) =>
              StringUtils
                .concat(
                  Literal(StringConstant("Left(")),
                  StringUtils.applyToString(term),
                  Literal(StringConstant(")"))
                )
            },
            functionOnRight = { (tpe, term) =>
              StringUtils
                .concat(
                  Literal(StringConstant("Right(")),
                  StringUtils.applyToString(term),
                  Literal(StringConstant(")"))
                )
            }
          )
        }
    }
    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }
}
