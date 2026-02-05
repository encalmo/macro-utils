package org.encalmo.utils

import scala.quoted.*

import JavaMapUtils.*

object JavaMapUtilsTestMacro {

  inline def testMaybeVisitJavaMap[K, V](inline value: java.util.Map[K, V]): String = {
    ${ testMaybeVisitJavaMapImpl[K, V]('{ value }) }
  }

  def testMaybeVisitJavaMapImpl[K: Type, V: Type](valueExpr: Expr[java.util.Map[K, V]])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testMaybeVisitJavaMap2Impl[K, V](valueExpr)
  }

  def testMaybeVisitJavaMap2Impl[K: Type, V: Type](
      valueExpr: Expr[java.util.Map[K, V]]
  )(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TypeRepr.of[java.util.Map[K, V]] match {
      case TypeReprIsJavaMap(keyTpe, valueTpe) =>
        cache.put(
          buildMapLoop(
            TypeNameUtils.valueNameOf(keyTpe),
            keyTpe,
            valueTpe,
            valueExpr.asTerm,
            functionOnEntry = { (key, value) =>
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
