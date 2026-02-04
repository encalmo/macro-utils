package org.encalmo.utils

import MapUtils.*

import scala.quoted.*

object MapUtilsTestMacro {

  inline def testBuildMapLoop[K, V](value: Map[K, V]): String = {
    ${ testBuildMapLoopImpl[K, V]('{ value }) }
  }

  def testBuildMapLoopImpl[K: Type, V: Type](valueExpr: Expr[Map[K, V]])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testBuildMapLoop2Impl[K, V](valueExpr)
  }

  def testBuildMapLoop2Impl[K: Type, V: Type](using
      cache: StatementsCache
  )(valueExpr: Expr[Map[K, V]]): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    cache.put {
      buildMapLoop[K, V](
        "testIterator",
        valueExpr.asTerm,
        onItem = { [K: Type, V: Type] => (key, value) =>
          bufferRef.methodCall(
            "append",
            List(
              StringUtils.concat(
                StringUtils.applyToString(key),
                Literal(StringConstant(" -> ")),
                StringUtils.applyToString(value)
              )
            )
          )
        }
      )
    }

    cache.put {
      bufferRef.methodCall("mkString", List(Literal(StringConstant(", "))))
    }

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }
}
