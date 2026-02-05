package org.encalmo.utils

import SelectableUtils.*
import scala.quoted.*

object SelectableUtilsTestMacro {

  inline def testMaybeVisitSelectable[T](value: T): String = {
    ${ testMaybeVisitSelectableImpl[T]('{ value }) }
  }

  def testMaybeVisitSelectableImpl[T: Type](valueExpr: Expr[T])(using Quotes): Expr[String] = {
    given StatementsCache = new StatementsCache
    testMaybeVisitSelectable2Impl[T](valueExpr)
  }

  def testMaybeVisitSelectable2Impl[T: Type](valueExpr: Expr[T])(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TypeRepr.of[T] match {
      case TypeReprIsSelectable(fieldsTpe) =>

        maybeVisitSelectable(
          TypeRepr.of[T],
          functionWhenSelectable = { fieldsTpe =>
            visitFields(using cache)(
              fieldsTpe,
              valueExpr.asTerm,
              functionOnField = { (tpe, name, value) =>
                cache.put {
                  val messageTerm =
                    StringUtils.concat(
                      Literal(StringConstant(name)),
                      Literal(StringConstant(": ")),
                      Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                      Literal(StringConstant(" = ")),
                      StringUtils.applyToString(value)
                    )
                  MethodUtils.methodCall(using cache)(bufferRef, "append", List(messageTerm))
                }
              }
            )
          }
        )
      case _ => ()
    }

    cache.getBlockExprOf(
      MethodUtils
        .methodCall(
          targetTerm = MethodUtils.methodCall(targetTerm = bufferRef, methodName = "toList", argTerms = Nil),
          methodName = "mkString",
          argTerms = List(Literal(StringConstant(", ")))
        )
        .asExprOf[String]
    )
  }

  inline def testMaybeTransformSelectableIntoBlockOfUnit[T](value: T): String = {
    ${ testMaybeTransformSelectableIntoBlockOfUnitImpl[T]('{ value }) }
  }

  def testMaybeTransformSelectableIntoBlockOfUnitImpl[T: Type](valueExpr: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[String]
    maybeTransformSelectableIntoBlockOfUnit[T](
      functionExpr = { [Fields: Type] => Quotes ?=>
        transformFieldsIntoBlockOfUnit[T, Fields](
          valueExpr,
          functionExpr = { [A: Type] => (name, value) =>
            buffer.append(name.valueOrAbort + ": " + TypeRepr.of[A].show(using Printer.TypeReprShortCode))
            '{}
          }
        )
      }
    ).getOrElse('{})
    Expr(buffer.mkString(", "))
  }

}
