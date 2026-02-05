package org.encalmo.utils

import UnionUtils.*
import scala.quoted.*

object UnionUtilsTestMacro {

  inline def testIsUnion[A]: Boolean = ${ testIsUnionImpl[A] }
  def testIsUnionImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(UnionUtils.isUnion[A])
  }

  inline def testTransformToMatchExpressionMethod[A](value: A): List[String] = {
    ${ testTransformToMatchExpressionMethodImpl[A]('{ value }) }
  }

  def testTransformToMatchExpressionMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    transformToMatchExpression[A](
      Expr("union"),
      valueExpr,
      { [A: Type] => (name, value) =>
        val expr = '{
          "case _: " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " =>"
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
  }

  inline def testTransformToMatchTermMethod[A](value: A): String = {
    ${ testTransformToMatchTermMethodImpl[A]('{ value }) }
  }

  def testTransformToMatchTermMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testTransformToMatchTermMethod2Impl[A](valueExpr)
  }

  def testTransformToMatchTermMethod2Impl[A: Type](
      valueExpr: Expr[A]
  )(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    cache.put {
      transformToMatchTerm[A](
        valueExpr.asTerm,
        functionExpr = { [A: Type] => value =>
          StringUtils.concat(
            Literal(StringConstant("case _: ")),
            Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode))),
            Literal(StringConstant(" => ")),
            value
          )
        }
      )
    }
    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }
}
