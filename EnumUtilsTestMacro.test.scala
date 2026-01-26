package org.encalmo.utils


import EnumUtils.*
import scala.quoted.*

object EnumUtilsTestMacro {

  inline def testIsEnumOrSealedADT[A]: Boolean = ${ testIsEnumOrSealedADTImpl[A] }
  def testIsEnumOrSealedADTImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(EnumUtils.isEnumOrSealedADT[A])
  }

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    visit[A](
      valueExpr,
      functionWhenCaseValueExpr = { [A: Type] => Quotes ?=> (name, value, annotations) =>
        val expr = '{
          "case " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " =>"
        }
        buffer += expr
        '{}
      },
      functionWhenCaseClassExpr = { [A: Type] => Quotes ?=> (name, value, annotations) =>
        val expr = '{
          "case _: " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " =>"
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
  }

}
