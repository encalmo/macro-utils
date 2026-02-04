package org.encalmo.utils

import scala.quoted.*
import StatementsCache.*

object StatementsCacheTestMacro {

  inline def testCreateNestedScope(): String = {
    ${ testCreateNestedScopeImpl() }
  }

  def testCreateNestedScopeImpl()(using Quotes): Expr[String] = {
    val cache: StatementsCache = new StatementsCache
    testCreateNestedScope2Impl(using cache)
  }

  def testCreateNestedScope2Impl(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    cache.put(bufferRef.methodCall("append", List(stringLiteral("Outer!"))))

    val nested = cache.createNestedScope()
    nested.putMethodCallOf[Unit](
      "foo",
      List("x"),
      List(nested.quotes.reflect.TypeRepr.of[String]),
      List(nested.stringLiteral("ouch")),
      methodBody ?=>
        arguments => {
          methodBody.putMethodCallOf[Unit](
            "bar",
            List("y"),
            List(methodBody.quotes.reflect.TypeRepr.of[String]),
            List(methodBody.stringLiteral("puff")),
            methodBody2 ?=>
              arguments2 => {
                methodBody2.put(
                  nested
                    .getValueRef("buffer")
                    .toTerm
                    .methodCall(
                      "append", {
                        arguments2.map(a =>
                          methodBody2.quotes.reflect.Ref(a.symbol.asInstanceOf[methodBody2.quotes.reflect.Symbol])
                        )
                      }
                    )
                )
              },
            scope = StatementsCache.Scope.TopLevel
          )
          methodBody.putMethodCall("bar", List(methodBody.stringLiteral("few")))
        },
      scope = StatementsCache.Scope.TopLevel
    )

    cache.putMethodCall("bar", List(cache.stringLiteral("huhu")))
    cache.put(nested.asTermOf(cache))
    cache.put(bufferRef.methodCall("append", List(stringLiteral("Outer!"))))
    cache.put(bufferRef.methodCall("mkString", List(stringLiteral(", "))))

    val result = cache.asTerm
    // import cache.quotes.reflect.*
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

  inline def testCreateEmptyNestedScope(): String = {
    ${ testCreateEmptyNestedScopeImpl() }
  }

  def testCreateEmptyNestedScopeImpl()(using Quotes): Expr[String] = {
    val cache: StatementsCache = new StatementsCache
    testCreateEmptyNestedScope2Impl(using cache)
  }

  def testCreateEmptyNestedScope2Impl(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    cache.put(bufferRef.methodCall("append", List(stringLiteral("Outer!"))))

    val nested = cache.createNestedScope()
    nested.putParamlessMethodCallOf[Unit](
      "foo",
      methodBody ?=> (),
      scope = StatementsCache.Scope.Local
    )

    cache.put(nested.asTermOf(cache))
    cache.put(bufferRef.methodCall("append", List(stringLiteral("Outer!"))))
    cache.put(bufferRef.methodCall("mkString", List(stringLiteral(", "))))

    val result = cache.asTerm
    // import cache.quotes.reflect.*
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

}
