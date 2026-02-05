package org.encalmo.utils

import JavaIterableUtils.*

import scala.quoted.*

object JavaIterableTestMacro {
  inline def testMaybeVisitJavaIterable[A](inline value: A): String = {
    ${ testMaybeVisitJavaIterableImpl[A]('{ value }) }
  }

  def testMaybeVisitJavaIterableImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testMaybeVisitJavaIterable2Impl[A](valueExpr)
  }

  def testMaybeVisitJavaIterable2Impl[A: Type](valueExpr: Expr[A])(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TypeRepr.of[A] match {
      case TypeReprIsJavaIterable(tpe) =>
        cache.put(
          buildIterableLoop(
            TypeNameUtils.valueNameOf(tpe),
            tpe,
            valueExpr.asTerm,
            functionOnItem = { (tpe, term) =>
              bufferRef.methodCall("append", List(StringUtils.applyToString(term)))
            }
          )
        )
      case _ =>
        report.warning("not a java iterable type")
    }

    cache.put {
      bufferRef.methodCall("mkString", List(Literal(StringConstant(", "))))
    }

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }
}
