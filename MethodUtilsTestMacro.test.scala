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

  inline def testCallOrBuildMethodOfUnitWithCache(
      methodName: String
  ): Unit = {
    ${ testCallOrBuildMethodOfUnitWithCacheImpl('{ methodName }) }
  }

  def testCallOrBuildMethodOfUnitWithCacheImpl(
      methodNameExpr: Expr[String]
  )(using Quotes): Expr[Unit] = {
    val cache = new StatementsCache
    cache.putMethodCallOf[Unit](
      methodNameExpr.valueOrAbort,
      Nil,
      Nil,
      Nil,
      { (nested: StatementsCache) ?=> arguments =>
        import nested.quotes.reflect.*
        nested.put {
          MethodUtils
            .callPrintln(using nested)(Literal(StringConstant("Hello World!")))
        }
      }
    )
    cache.putMethodCallOf[Unit](
      methodNameExpr.valueOrAbort,
      Nil,
      Nil,
      Nil,
      { (nested: StatementsCache) ?=> arguments =>
        import nested.quotes.reflect.*
        nested.put {
          MethodUtils
            .callPrintln(using nested)(Literal(StringConstant("Hello World!")))
        }
      }
    )
    cache.getBlockExprOfUnit
  }
}
