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

  inline def testCollectMethod[A](value: A): List[String] = {
    ${ testCollectMethodImpl[A]('{ value }) }
  }

  def testCollectMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    collect[A](
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

  inline def testVisitMethod[A](value: A): List[String] = {
    ${ testVisitMethodImpl[A]('{ value }) }
  }

  def testVisitMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    given cache: StatementsCache = new StatementsCache
    testVisitMethod2Impl[A](valueExpr)
  }

  def testVisitMethod2Impl[A: Type](valueExpr: Expr[A])(using cache: StatementsCache): Expr[List[String]] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getOrElseCreateValueRef("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    visit[A](
      Some("tuple"),
      valueExpr.asTerm,
      functionWhenTupleExpr = { [A: Type] => (name, value, index) =>
        {
          cache.addStatement {
            val messageTerm = StringUtils.concat(
              Literal(StringConstant("tuple element at ")),
              Literal(IntConstant(index)),
              Literal(StringConstant(": ")),
              Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode))),
              Literal(StringConstant(" = ")),
              StringUtils.applyToString(value)
            )
            MethodUtils.callMethod(bufferRef, "append", List(messageTerm))
          }
        }
      },
      functionWhenNamedTupleExpr = { [A: Type] => (name, value, index) =>
        {
          cache.addStatement {
            val messageTerm = StringUtils.concat(
              Literal(StringConstant("named tuple element ")),
              Literal(StringConstant(name.getOrElse("unknown"))),
              Literal(StringConstant(": ")),
              Literal(StringConstant(TypeRepr.of[A].show(using Printer.TypeReprShortCode))),
              Literal(StringConstant(" = ")),
              StringUtils.applyToString(value)
            )
            MethodUtils.callMethod(bufferRef, "append", List(messageTerm))
          }
        }
      },
      onStart = '{}.asTerm,
      onEnd = '{}.asTerm
    )
    cache.getBlockExprOf(
      MethodUtils.callMethod(targetTerm = bufferRef, methodName = "toList", argTerms = Nil).asExprOf[List[String]]
    )
  }
}
