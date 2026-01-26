package org.encalmo.utils


import UnionUtils.*
import scala.quoted.*

object UnionUtilsTestMacro {

  inline def testIsUnion[A]: Boolean = ${ testIsUnionImpl[A] }
  def testIsUnionImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(UnionUtils.isUnion[A])
  }

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    visit[A](
      Expr("union"),
      valueExpr,
      { [A: Type] => Quotes ?=> (name, value) =>
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
