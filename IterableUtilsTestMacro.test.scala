package org.encalmo.utils

import IterableUtils.*

import scala.quoted.*

object IterableUtilsTestMacro {

  inline def testBuildIterableLoop[A <: Tuple](value: A): String = {
    ${ testBuildIterableLoopImpl[A]('{ value }) }
  }

  def testBuildIterableLoopImpl[A <: Tuple: Type](valueExpr: Expr[Tuple])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    testBuildIterableLoop2Impl[A](valueExpr.asTerm)
  }

  def testBuildIterableLoop2Impl[A <: Tuple: Type](using
      cache: StatementsCache
  )(valueTerm: cache.quotes.reflect.Term): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TupleUtils.visit(
      label = Some("test"),
      tpe = TypeRepr.of[A],
      valueTerm = valueTerm,
      functionWhenTuple = {
        (tpe, name, value, index) =>
          tpe match {
            case TypeReprIsIterable(itemType) =>
              cache.put {
                buildIterableLoop(
                  "testIterator_" + index,
                  itemType,
                  value,
                  functionOnItem = { (tpe, term) =>
                    bufferRef.methodCall("append", List(StringUtils.applyToString(term)))
                  }
                )
              }
          }

        '{}
      },
      functionWhenNamedTuple = { (tpe, name, value, index) =>
        '{}.asTerm
      },
      onStart = '{}.asTerm,
      onEnd = '{}.asTerm
    )

    cache.put {
      bufferRef.methodCall("mkString", List(Literal(StringConstant(", "))))
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
    val result =
      createStaticList(TypeRepr.of[String], termExpr.valueOrAbort.toList.map(c => Literal(StringConstant(c.toString))))
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[List[String]]
  }

}
