package org.encalmo.utils

import MethodUtils.*
import scala.quoted.*

object MethodUtilsTestMacro {

  inline def testMaybeSelectedValue[T](selector: String, expr: T): String = {
    ${ testMaybeSelectedValueImpl[T]('{ selector }, '{ expr }) }
  }

  def testMaybeSelectedValueImpl[T: Type](selectorExpr: Expr[String], exprExpr: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    '{
      var result: String = ""
      ${
        maybeSelectedValue(
          selectorExpr.valueOrAbort,
          selectorExpr,
          exprExpr,
          functionExpr = { [A: Type] => Quotes ?=> (name, value) =>
            '{
              result = ${ name } + ": " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) }
                + " = " + ${ value }
            }
          },
          fallbackExpr = '{ result = "selector not found" }
        )
      }
      result
    }
  }

  inline def testWrapInMethodCallWithCache(methodName: String, methodBody: Unit): Unit = {
    ${ testWrapInMethodCallWithCacheImpl('{ methodName }, '{ methodBody }) }
  }

  def testWrapInMethodCallWithCacheImpl(
      methodNameExpr: Expr[String],
      methodBodyExpr: Expr[Unit]
  )(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val cache = new MethodsCache
    cache.getOrElseCreateMethod(methodNameExpr.valueOrAbort, methodBodyExpr)
    cache.getOrElseCreateMethod(methodNameExpr.valueOrAbort, methodBodyExpr)
    cache.getOrElseCreateMethod(methodNameExpr.valueOrAbort, methodBodyExpr)
    cache.getOrElseCreateMethod(methodNameExpr.valueOrAbort, methodBodyExpr)
    val result = cache.getBlockExpr
    report.warning(result.asTerm.show(using Printer.TreeStructure))
    result
  }
}
