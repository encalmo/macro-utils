package org.encalmo.utils

import MethodUtils.*
import scala.quoted.*

object MethodUtilsTestMacro {

  inline def testMaybeSelectedValue[T](selector: String, expr: T): String = {
    ${ testMaybeSelectedValueImpl[T]('{ selector }, '{ expr }) }
  }

  def testMaybeSelectedValueImpl[T: Type](selectorExpr: Expr[String], expr: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    '{
      var result: String = ""
      ${
        maybeSelectExpr(
          selectorExpr.valueOrAbort,
          expr,
          functionExpr = { [A: Type] => valueExpr =>
            val selected = Expr(valueExpr.asTerm.show(using Printer.TreeCode))
            '{
              result = ${ selected } + ": " + ${
                Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode))
              } + " = " + ${ valueExpr }
            }
          }
        ).getOrElse('{ result = "selector not found" })
      }
      result
    }
  }

  inline def testMaybeSelectTerm[T](selector: String, expr: T): String = {
    ${ testMaybeSelectTermImpl[T]('{ selector }, '{ expr }) }
  }

  def testMaybeSelectTermImpl[T: Type](selectorExpr: Expr[String], exprExpr: Expr[T])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    testMaybeSelectTerm2Impl[T](selectorExpr.valueOrAbort, exprExpr.asTerm)
  }

  def testMaybeSelectTerm2Impl[T: Type](using
      cache: StatementsCache
  )(selector: String, valueTerm: cache.quotes.reflect.Term): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    maybeSelectTerm(
      selector,
      TypeRepr.of[T],
      valueTerm,
      functionWhenSelected = { (tpe, term) =>
        cache.put {
          StringUtils.concat(
            Literal(StringConstant(valueTerm.show(using Printer.TreeCode))),
            Literal(StringConstant(".")),
            Literal(StringConstant(selector)),
            Literal(StringConstant(": ")),
            Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
            Literal(StringConstant(" = ")),
            term
          )
        }
      }
    )

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
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

  class Testy(id: String) {
    def foo(a: Int, b: Int)(c: String): String = {
      id + ":" + c + a + "-" + b
    }
  }

  inline def testMethodCall(testy: Testy): String = {
    ${ testMethodCallImpl('{ testy }) }
  }

  def testMethodCallImpl(testyExpr: Expr[Testy])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    testMethodCall2Impl(testyExpr.asTerm)
  }

  def testMethodCall2Impl(using cache: StatementsCache)(testyTerm: cache.quotes.reflect.Term): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    cache.put {
      testyTerm.methodCall(
        "foo",
        List(Literal(IntConstant(1)), Literal(IntConstant(2))),
        List(Literal(StringConstant("foo")))
      )
    }
    cache.getBlockExprOf[String]
  }
}
