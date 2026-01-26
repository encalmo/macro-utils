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

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    visit[A](
      valueExpr,
      { [A: Type] => Quotes ?=> (name, value, annotations) =>
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

  inline def testTransformMethod[A](value: A): List[String] = {
    ${ testTransformMethodImpl[A]('{ value }) }
  }

  def testTransformMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    Expr.ofList(
      transform[A, String](
        valueExpr,
        { [A: Type] => Quotes ?=> (name, value, annotations) =>
          Some('{
            ${ name } + ": " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " = " + ${
              value
            }.toString
          })
        }
      )
    )
  }
}
