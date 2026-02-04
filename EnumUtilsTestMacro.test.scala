package org.encalmo.utils

import EnumUtils.*
import scala.quoted.*

object EnumUtilsTestMacro {

  inline def testIsEnumOrSealedADT[A]: Boolean = ${ testIsEnumOrSealedADTImpl[A] }
  def testIsEnumOrSealedADTImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(EnumUtils.isEnumOrSealedADT[A])
  }

  inline def testTransformToExprMethod[A](value: A): List[String] = {
    ${ testTransformToExprMethodImpl[A]('{ value }) }
  }

  def testTransformToExprMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    transformToMatchExpression[A](
      valueExpr,
      functionWhenCaseValueExpr = { [A: Type] => (name, value, annotations) =>
        val expr = '{
          "case " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " =>"
        }
        buffer.append(expr)
        '{}
      },
      functionWhenCaseClassExpr = { [A: Type] => (name, value, annotations) =>
        val expr = '{
          "case _: " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " =>"
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
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

    cache.put(
      transformToMatchTerm[A](
        valueExpr.asTerm,
        functionWhenCaseValueExpr = { [A: Type] => (name, value, annotations) =>
          StringUtils.concat(
            Literal(StringConstant("case ")),
            Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode))),
            Literal(StringConstant(" => ")),
            value
          )
        },
        functionWhenCaseClassExpr = { [A: Type] => (name, value, annotations) =>
          StringUtils.concat(
            Literal(StringConstant("case _: ")),
            Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode))),
            Literal(StringConstant(" => ")),
            value
          )
        }
      )
    )

    cache.getBlockExprOf[String]
  }

}
