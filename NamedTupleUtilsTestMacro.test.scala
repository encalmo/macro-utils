package org.encalmo.utils

import NamedTupleUtils.*
import scala.quoted.*

object NamedTupleUtilsTestMacro {

  inline def testIsNamedTuple[A]: Boolean = ${ testIsNamedTupleImpl[A] }
  def testIsNamedTupleImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(NamedTupleUtils.isNamedTuple[A])
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

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TypeRepr.of[A] match {
      case TypeReprIsNamedTuple() =>

        visit(
          Some("tuple"),
          TypeRepr.of[A],
          valueExpr.asTerm,
          functionOnField = { (tpe, name, value, index) =>
            {
              cache.put {
                val messageTerm = StringUtils.concat(
                  Literal(StringConstant("named tuple element ")),
                  Literal(StringConstant(name)),
                  Literal(StringConstant(": ")),
                  Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                  Literal(StringConstant(" = ")),
                  StringUtils.applyToString(value)
                )
                MethodUtils.methodCall(bufferRef, "append", List(messageTerm))
              }
            }
          }
        )
    }

    cache.getBlockExprOf(
      MethodUtils.methodCall(targetTerm = bufferRef, methodName = "toList", argTerms = Nil).asExprOf[List[String]]
    )
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
      functionOnField = { [A: Type] => Quotes ?=> (name, value, index) =>
        val typeName = TypeRepr.of[A].show(using Printer.TypeReprShortCode)
        val expr = '{
          "named tuple element " + ${ name.getOrElse(Expr("unknown")) } + ": " + ${ Expr(typeName) } + " = " + ${
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
