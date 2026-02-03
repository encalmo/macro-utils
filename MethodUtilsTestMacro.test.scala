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
          functionExpr = { [A: Type] => (name, value) =>
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

  inline def testAddMethodCallWithCache(inline times: Int, methodName: String, methodBody: => Unit): Unit = {
    ${ testAddMethodCallWithCacheImpl('{ times }, '{ methodName }, '{ methodBody }) }
  }

  def testAddMethodCallWithCacheImpl(
      timesExpr: Expr[Int],
      methodNameExpr: Expr[String],
      methodBodyExpr: Expr[Unit]
  )(using Quotes): Expr[Unit] = {
    val cache = new StatementsCache
    for (i <- 0 until timesExpr.valueOrAbort) {
      cache.addMethodCall(methodNameExpr.valueOrAbort, methodBodyExpr)
    }
    cache.getBlockExprOfUnit
  }

  inline def testCallOrBuildMethodOfUnitWithCache(
      methodName: String
  ): Unit = {
    ${ testCallOrBuildMethodOfUnitWithCacheImpl('{ methodName }) }
  }

  def testCallOrBuildMethodOfUnitWithCacheImpl(
      methodNameExpr: Expr[String]
  )(using Quotes): Expr[Unit] = {
    val cache = new StatementsCache
    cache.callOrBuildMethodOfUnit(
      methodNameExpr.valueOrAbort,
      { (nested: StatementsCache) ?=>
        import nested.quotes.reflect.*
        nested.addStatement {
          MethodUtils
            .callPrintln(using nested.quotes)(Literal(StringConstant("Hello World!")))
        }
      }
    )
    cache.callOrBuildMethodOfUnit(
      methodNameExpr.valueOrAbort,
      { (nested: StatementsCache) ?=>
        import nested.quotes.reflect.*
        nested.addStatement {
          MethodUtils
            .callPrintln(using nested.quotes)(Literal(StringConstant("Hello World!")))
        }
      }
    )
    cache.getBlockExprOfUnit
  }
}
