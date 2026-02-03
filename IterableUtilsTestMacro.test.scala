package org.encalmo.utils

import IterableUtils.*
import QuotesUtils.*
import scala.quoted.*

object IterableUtilsTestMacro {

  inline def testBuildIterableLoop[A](value: Iterable[A]): String = {
    ${ testBuildIterableLoopImpl[A]('{ value }) }
  }

  def testBuildIterableLoopImpl[A: Type](valueExpr: Expr[Iterable[A]])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testBuildIterableLoop2Impl[A](valueExpr)
  }

  def testBuildIterableLoop2Impl[A: Type](using cache: StatementsCache)(valueExpr: Expr[Iterable[A]]): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    cache.addStatement {
      buildIterableLoop[A](
        valueExpr.asTerm,
        onItem = { [A: Type] => term =>
          bufferRef.callMethod("append", List(StringUtils.applyToString(term)))
        }
      )
    }

    cache.addStatement {
      bufferRef.callMethod("mkString", List(Literal(StringConstant(", "))))
    }

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

  inline def testCreateStaticList(term: String): List[String] = {
    ${ testCreateStaticListImpl('{ term }) }
  }

  def testCreateStaticListImpl(termExpr: Expr[String])(using Quotes): Expr[List[String]] = {
    given cache: StatementsCache = new StatementsCache
    testCreateStaticList2Impl(termExpr)
  }

  def testCreateStaticList2Impl(using cache: StatementsCache)(termExpr: Expr[String]): Expr[List[String]] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    val result = createStaticList[String](termExpr.valueOrAbort.toList.map(c => Literal(StringConstant(c.toString))))
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[List[String]]
  }

}
