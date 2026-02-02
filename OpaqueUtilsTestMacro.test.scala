package org.encalmo.utils

import OpaqueTypeUtils.*
import scala.quoted.*

object OpaqueUtilsTestMacro {

  inline def testTransformToMatchExpression[A](value: A): List[String] = {
    ${ testTransformToMatchExpressionMethodImpl[A]('{ value }) }
  }

  def testTransformToMatchExpressionMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    transformToMatchExpression[A](
      Expr("opaque"),
      valueExpr,
      functionWhenOpaqueTypeExpr = { [A: Type] => (name, value) =>
        val expr = '{
          "opaque type with an upper bound of " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) }
        }
        buffer += expr
        '{}
      },
      functionWhenOtherExpr = { [A: Type] => (name, value) =>
        val expr = '{
          "not an opaque type " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) }
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
  }

  inline def testVisit[A](value: A): String = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testVisitMethod2Impl[A](valueExpr)
  }

  def testVisitMethod2Impl[A: Type](valueExpr: Expr[A])(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    visit[A](
      "opaque",
      valueExpr.asTerm,
      functionWhenOpaqueTypeExpr = { [A: Type] => (name, value) =>
        cache.addStatement(
          StringUtils.concat(
            Literal(StringConstant("opaque type with an upper bound of ")),
            Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode)))
          )
        )
      },
      functionWhenOtherExpr = { [A: Type] => (name, value) =>
        cache.addStatement(
          StringUtils.concat(
            Literal(StringConstant("not an opaque type ")),
            Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode)))
          )
        )
      }
    )

    cache.getBlockExprOf[String]
  }

}
