package org.encalmo.utils

import OpaqueTypeUtils.*
import scala.quoted.*

object OpaqueUtilsTestMacro {

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    visit[A](
      Expr("opaque"),
      valueExpr,
      functionWhenOpaqueTypeExpr = { [A: Type] => (name, value) =>
        val expr = '{
          "opaque type with an upper bound of " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) }
        }
        buffer += expr
        '{}
      },
      functionWhenOtherExpr = { [A: Type] => (name, value) =>
        val expr = '{
          "not an opaque type " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) }
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
  }

}
