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
    nested.putMethodOfUnitCall(
      "foo",
      methodBody ?=> {
        methodBody.putMethodOfUnitCall(
          "bar",
          methodBody2 ?=>
            methodBody2.put(nested.getValueRef("buffer").term.methodCall("append", List(stringLiteral("Nested!")))),
          scope = StatementsCache.Scope.TopLevel
        )
        methodBody.putMethodCall("bar")
      },
      scope = StatementsCache.Scope.TopLevel
    )

    cache.putMethodCall("bar")
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
    nested.putMethodOfUnitCall(
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
