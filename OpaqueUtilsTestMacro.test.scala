package org.encalmo.utils

import OpaqueTypeUtils.*
import scala.quoted.*

object OpaqueUtilsTestMacro {

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

    visit(
      "opaque",
      TypeRepr.of[A],
      valueExpr.asTerm,
      functionWhenOpaqueType = { (tpe, name, value) =>
        cache.put(
          StringUtils.concat(
            Literal(StringConstant("opaque type with an upper bound of ")),
            Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode)))
          )
        )
      },
      functionOtherwise = { (tpe, name, value) =>
        cache.put(
          StringUtils.concat(
            Literal(StringConstant("not an opaque type ")),
            Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode)))
          )
        )
      }
    )

    cache.getBlockExprOf[String]
  }

}
