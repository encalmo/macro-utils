package org.encalmo.utils

import EnumUtils.*
import scala.quoted.*

object EnumUtilsTestMacro {

  inline def testIsEnumOrSealedADT[A]: Boolean = ${ testIsEnumOrSealedADTImpl[A] }
  def testIsEnumOrSealedADTImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(EnumUtils.isEnumOrSealedADT[A])
  }

  inline def testTransformToTermMethod[A](value: A): String = {
    ${ testTransformToTermMethodImpl[A]('{ value }) }
  }

  def testTransformToTermMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given StatementsCache = new StatementsCache
    testTransformToTermMethod2Impl[A](valueExpr)
  }

  def testTransformToTermMethod2Impl[A: Type](valueExpr: Expr[A])(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    TypeRepr.of[A] match {
      case TypeReprIsEnum() =>
        cache.put(
          transformToMatchTerm(
            TypeRepr.of[A],
            valueExpr.asTerm,
            functionWhenCaseValue = { (tpe, name, value, annotations) =>
              StringUtils.concat(
                Literal(StringConstant("case ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                Literal(StringConstant(" => ")),
                value
              )
            },
            functionWhenCaseClass = { (tpe, name, value, annotations) =>
              StringUtils.concat(
                Literal(StringConstant("case _: ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                Literal(StringConstant(" => ")),
                value
              )
            }
          )
        )
    }

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

  inline def visitTermless[T]: String = {
    ${ visitTermlessImpl[T] }
  }

  def visitTermlessImpl[T: Type](using quotes: Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    visitTermlessUsingTypeTreeIterator(using cache)(tpe = TypeRepr.of[T])
  }

  def visitTermlessUsingTypeTreeIterator(using
      cache: StatementsCache
  )(tpe: cache.quotes.reflect.TypeRepr): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*
    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })
    EnumUtils.visitTermless(using cache)(
      tpe = tpe,
      functionWhenCaseValue = { (tpe, name, annotations) =>
        cache.put {
          bufferRef.methodCall(
            "append",
            List(
              StringUtils.concat(
                Literal(StringConstant("value case ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode)))
              )
            )
          )
        }
      },
      functionWhenCaseClass = { (tpe, name, annotations) =>
        cache.put {
          bufferRef.methodCall(
            "append",
            List(
              StringUtils.concat(
                Literal(StringConstant("case class ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode)))
              )
            )
          )
        }
      }
    )
    cache.put {
      bufferRef.methodCall("mkString", List(Literal(StringConstant(", "))))
    }
    cache.asExprOf[String]
  }

}
