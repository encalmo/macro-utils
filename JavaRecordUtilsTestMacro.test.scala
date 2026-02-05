package org.encalmo.utils

import JavaRecordUtils.*

import scala.quoted.*

object JavaRecordUtilsTestMacro {

  inline def testCollectMethod[A](value: A): List[String] = {
    ${ testCollectMethodImpl[A]('{ value }) }
  }

  def testCollectMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    collect[A](
      Expr("java record"),
      valueExpr,
      functionExpr = { [A: Type] => (name, value) =>
        val expr = '{
          ${ name } + ": " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " = " + ${
            value
          }.toString
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
  }

  inline def testVisitMethod[A](value: A): String = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testVisitMethod2Impl[A](valueExpr)
  }

  def testVisitMethod2Impl[A: Type](valueExpr: Expr[A])(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TypeRepr.of[A] match {
      case TypeReprIsJavaRecord() =>

        visit(
          "java record",
          TypeRepr.of[A],
          valueExpr.asTerm,
          functionOnField = { (tpe, name, value) =>
            cache.put {
              val messageTerm = StringUtils.concat(
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
    }

    cache.getBlockExprOf(
      bufferRef
        .methodCall("toList", Nil)
        .methodCall("mkString", List(Literal(StringConstant(", "))))
        .asExprOf[String]
    )
  }
}
