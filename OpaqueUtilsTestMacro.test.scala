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

    cache.asExprOf[String]
  }

  inline def testFindBaseTypeFromUnapply[A]: String = {
    ${ testFindBaseTypeFromUnapplyImpl[A] }
  }

  def testFindBaseTypeFromUnapplyImpl[A: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(findBaseTypeFromUnapply(TypeRepr.of[A]).map(_.show(using Printer.TypeReprShortCode)).getOrElse("<none>"))
  }

  inline def testFindBaseTypeFromApply[A]: String = {
    ${ testFindBaseTypeFromApplyImpl[A] }
  }

  def testFindBaseTypeFromApplyImpl[A: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(findBaseTypeFromApply(TypeRepr.of[A]).map(_.show(using Printer.TypeReprShortCode)).getOrElse("<none>"))
  }

  inline def testVisitTermless[A]: String = {
    ${ testVisitTermlessMethodImpl[A] }
  }

  def testVisitTermlessMethodImpl[A: Type](using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testVisitTermlessMethod2Impl[A]
  }

  def testVisitTermlessMethod2Impl[A: Type](using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    visitTermless(
      "opaque",
      TypeRepr.of[A],
      functionWhenOpaqueType = { (tpe, name) =>
        cache.put(
          StringUtils.concat(
            Literal(StringConstant("opaque type with an upper bound of ")),
            Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode)))
          )
        )
      },
      functionOtherwise = { (tpe, name) =>
        cache.put(
          StringUtils.concat(
            Literal(StringConstant("not an opaque type ")),
            Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode)))
          )
        )
      }
    )

    cache.asExprOf[String]
  }

}
