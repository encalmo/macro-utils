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
      methodName = "foo",
      parameterNames = List("x"),
      parameterTypes = List(nested.quotes.reflect.TypeRepr.of[String]),
      parameters = List(nested.stringLiteral("ouch")),
      minMethodLinesCount = 2,
      buildMethodBody = methodBody ?=>
        arguments => {
          methodBody.putMethodCallOf[Unit](
            methodName = "bar",
            parameterNames = List("y"),
            parameterTypes = List(methodBody.quotes.reflect.TypeRepr.of[String]),
            parameters = List(methodBody.stringLiteral("puff")),
            minMethodLinesCount = 2,
            buildMethodBody = methodBody2 ?=>
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
      methodName = "foo",
      minMethodLinesCount = 2,
      buildMethodBody = methodBody ?=> (),
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

  inline def testCreateMethod(s: String): String = {
    ${ testCreateMethodImpl('{ s }) }
  }

  def testCreateMethodImpl(expr: Expr[String])(using Quotes): Expr[String] = {
    val cache: StatementsCache = new StatementsCache
    val term = testCreateMethod2Impl(using cache)(expr)
    // import cache.quotes.reflect.*
    // report.info(term.show(using Printer.TreeCode))
    term.asExprOf[String]
  }

  def testCreateMethod2Impl(using cache: StatementsCache)(expr: Expr[String]): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    cache
      .putMethodCallOf[String](
        methodName = "foo",
        parameterNames = List("x"),
        parameterTypes = List(TypeRepr.of[String]),
        parameters = List(expr.asTerm),
        minMethodLinesCount = 2,
        buildMethodBody = cache ?=>
          params =>
            cache.put(
              StringUtils.concat(
                stringLiteral("ouch"),
                params.headOption.match {
                  case Some(term: Term)  => term.toTerm
                  case Some(param: Tree) => Ref(param.symbol).toTerm
                  case None              => unit

                }
              )
            ),
        scope = StatementsCache.Scope.Local
      )

    cache.asTerm
  }

  inline def testCreateLargeMethod(s: String): String = {
    ${ testCreateLargeMethodImpl('{ s }) }
  }

  def testCreateLargeMethodImpl(expr: Expr[String])(using Quotes): Expr[String] = {
    val cache: StatementsCache = new StatementsCache
    val term = testCreateLargeMethod2Impl(using cache)(expr)
    // import cache.quotes.reflect.*
    // report.info(term.show(using Printer.TreeCode))
    term.asExprOf[String]
  }

  def testCreateLargeMethod2Impl(using cache: StatementsCache)(expr: Expr[String]): cache.quotes.reflect.Term = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    cache
      .putMethodCallOf[String](
        methodName = "foo",
        parameterNames = List("x"),
        parameterTypes = List(TypeRepr.of[String]),
        parameters = List(expr.asTerm),
        minMethodLinesCount = 10,
        buildMethodBody = cache ?=>
          params =>
            1 to 10 foreach { i =>
              cache.put(
                StringUtils.concat(
                  stringLiteral("ouch"),
                  params.headOption.match {
                    case Some(term: Term)  => term.toTerm
                    case Some(param: Tree) => Ref(param.symbol).toTerm
                    case None              => unit

                  },
                  stringLiteral(i.toString)
                )
              )
            },
        scope = StatementsCache.Scope.Local
      )

    cache.asTerm
  }

}
