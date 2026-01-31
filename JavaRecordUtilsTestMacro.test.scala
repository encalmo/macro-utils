package org.encalmo.utils

import JavaRecordUtils.*
import scala.quoted.*

object JavaRecordUtilsTestMacro {

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    visit[A](
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

}
