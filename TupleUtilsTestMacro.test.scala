package org.encalmo.utils

import TupleUtils.*
import scala.quoted.*

object TupleUtilsTestMacro {

  inline def testIsTuple[A]: Boolean = ${ testIsTupleImpl[A] }
  def testIsTupleImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(TupleUtils.isTuple[A])
  }

  inline def testIsNamedTuple[A]: Boolean = ${ testIsNamedTupleImpl[A] }
  def testIsNamedTupleImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(TupleUtils.isNamedTuple[A])
  }

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    visit[A](
      Some(Expr("tuple")),
      valueExpr,
      functionWhenTupleExpr = { [A: Type] => Quotes ?=> (name, value, index) =>
        val typeName = TypeRepr.of[A].show(using Printer.TypeReprShortCode)
        val expr = '{
          "tuple element at " + ${ Expr(index) } + ": " + ${ Expr(typeName) } + " = " + ${
            value
          }.toString
        }
        buffer += expr
        '{}
      },
      functionWhenNamedTupleExpr = { [A: Type] => Quotes ?=> (name, value, index) =>
        val typeName = TypeRepr.of[A].show(using Printer.TypeReprShortCode)
        val expr = '{
          "named tuple element " + ${ name.getOrElse(Expr("unknown")) } + ": " + ${ Expr(typeName) } + " = " + ${
            value
          }.toString
        }
        buffer += expr
        '{}
      },
      onStart = '{},
      onEnd = '{}
    )
    Expr.ofList(buffer.toList)
  }
}
