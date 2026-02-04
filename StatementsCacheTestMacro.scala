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
      methodBody ?=>
        methodBody.put(nested.getValueRef("buffer").term.methodCall("append", List(stringLiteral("Nested!"))))
    )

    cache.put(nested.asTermOf(cache))
    cache.put(bufferRef.methodCall("append", List(stringLiteral("Outer!"))))
    cache.put(bufferRef.methodCall("mkString", List(stringLiteral(", "))))

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

}
