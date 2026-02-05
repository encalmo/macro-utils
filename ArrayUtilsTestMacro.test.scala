package org.encalmo.utils

import ArrayUtils.*
import scala.quoted.*

object ArrayUtilsTestMacro {

  inline def testBuildArrayLoop[A](value: Array[A]): String = {
    ${ testBuildArrayLoopImpl[A]('{ value }) }
  }

  def testBuildArrayLoopImpl[A: Type](valueExpr: Expr[Array[A]])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    import cache.quotes.reflect.*
    testBuildArrayLoop2Impl[A](valueExpr.asTerm)
  }

  def testBuildArrayLoop2Impl[A: Type](using
      cache: StatementsCache
  )(valueTerm: cache.quotes.reflect.Term): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    cache.put {
      buildArrayLoop[A](
        valueTerm,
        onItem = { [A: Type] => term =>
          bufferRef.methodCall("append", List(StringUtils.applyToString(term)))
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

  inline def testCreateArrayViaList[A](inline value: List[A]): Array[A] = {
    ${ testCreateArrayViaListImpl[A]('{ value }) }
  }

  def testCreateArrayViaListImpl[A: Type](valueExpr: Expr[List[A]])(using Quotes): Expr[Array[A]] = {
    given cache: StatementsCache = new StatementsCache
    given cache.quotes.type = cache.quotes
    testCreateArrayViaList2Impl[A](TypeUtils.extractTermsFromList[A](valueExpr))
  }

  def testCreateArrayViaList2Impl[A: Type](using
      cache: StatementsCache
  )(valueTerms: List[cache.quotes.reflect.Term]): Expr[Array[A]] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    cache.put {
      createArrayViaList[A](valueTerms)
    }

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[Array[A]]
  }
}
