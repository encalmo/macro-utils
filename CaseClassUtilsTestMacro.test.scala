package org.encalmo.utils

import CaseClassUtils.*

import scala.quoted.*

object CaseClassUtilsTestMacro {

  inline def testIsCaseClass[A]: Boolean = ${ testIsCaseClassImpl[A] }
  def testIsCaseClassImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(CaseClassUtils.isCaseClass[A])
  }

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using outer: Quotes): Expr[List[String]] = {
    given cache: StatementsCache = new StatementsCache
    testVisitMethodImpl2[A](valueExpr)
  }

  def testVisitMethodImpl2(using cache: StatementsCache)[A: Type](valueExpr: Expr[A]): Expr[List[String]] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TypeRepr.of[A] match {
      case TypeReprIsCaseClass() =>
        visit(
          TypeRepr.of[A],
          valueExpr.asTerm,
          { (tpe, name, value, annotations) =>
            cache.put {
              val messageTerm =
                StringUtils.concat(
                  Literal(StringConstant(annotations.map(_.toString).mkString(", "))),
                  Literal(StringConstant(name)),
                  Literal(StringConstant(": ")),
                  Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                  Literal(StringConstant(" = ")),
                  StringUtils.applyToString(value)
                )
              MethodUtils.methodCall(bufferRef, "append", List(messageTerm))
            }
          }
        )
      case _ =>
        '{}
    }

    cache.getBlockExprOf(
      bufferRef.methodCall("toList", Nil).asExprOf[List[String]]
    )
  }

  inline def printCaseClassFields2[T](inline value: T): Unit = ${ printCaseClassFields2Impl('value) }

  def printCaseClassFields2Impl[T: Type](value: Expr[T])(using Quotes): Expr[Unit] = {
    given StatementsCache = new StatementsCache
    printCaseClassFields21Impl[T](value)
  }

  def printCaseClassFields21Impl[T: Type](valueExpr: Expr[T])(using cache: StatementsCache): Expr[Unit] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    CaseClassUtils.visit(using cache)(
      TypeRepr.of[T],
      valueExpr.asTerm,
      (tpe, name, value, annotations) =>
        val term =
          MethodUtils.callPrintln(using cache)(
            Literal(StringConstant(name)),
            Literal(StringConstant(": ")),
            StringUtils.applyToString(value)
          )
        cache.put(term)
    )
    cache.getBlockExprOfUnit
  }

}
