package org.encalmo.utils

import UnionUtils.*
import scala.quoted.*

object UnionUtilsTestMacro {

  inline def testIsUnion[A]: Boolean = ${ testIsUnionImpl[A] }
  def testIsUnionImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    Expr(UnionUtils.isUnion[A])
  }

  inline def testTransformToMatchExpressionMethod[A](value: A): List[String] = {
    ${ testTransformToMatchExpressionMethodImpl[A]('{ value }) }
  }

  def testTransformToMatchExpressionMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val buffer = collection.mutable.ListBuffer.empty[Expr[String]]
    transformToMatchExpression[A](
      Expr("union"),
      valueExpr,
      { [A: Type] => (name, value) =>
        val expr = '{
          "case _: " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " =>"
        }
        buffer += expr
        '{}
      }
    )
    Expr.ofList(buffer.toList)
  }

  inline def testTransformToMatchTermMethod[A](value: A): String = {
    ${ testTransformToMatchTermMethodImpl[A]('{ value }) }
  }

  def testTransformToMatchTermMethodImpl[A: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testTransformToMatchTermMethod2Impl[A](valueExpr)
  }

  def testTransformToMatchTermMethod2Impl[A: Type](
      valueExpr: Expr[A]
  )(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    TypeRepr.of[A] match {
      case TypeReprIsUnion(tpes) =>
        cache.put {
          transformToMatchTerm(
            TypeRepr.of[A],
            valueExpr.asTerm,
            functionOnCase = { (tpe, value) =>
              StringUtils.concat(
                Literal(StringConstant("case _: ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                Literal(StringConstant(" => ")),
                value
              )
            }
          )
        }
      case _ =>
        cache.put(Literal(StringConstant("not an union type")))
    }
    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

  inline def testTransformTupleToMatchTermMethod[A <: Tuple](value: A): String = {
    ${ testTransformTupleToMatchTermMethodImpl[A]('{ value }) }
  }

  def testTransformTupleToMatchTermMethodImpl[A <: Tuple: Type](valueExpr: Expr[A])(using Quotes): Expr[String] = {
    given cache: StatementsCache = new StatementsCache
    testTransformTupleToMatchTermMethod2Impl[A](valueExpr)
  }

  def testTransformTupleToMatchTermMethod2Impl[A <: Tuple: Type](
      valueExpr: Expr[A]
  )(using cache: StatementsCache): Expr[String] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val bufferRef = cache.getValueRefOfExpr("buffer", '{ collection.mutable.ListBuffer.empty[String] })

    TupleUtils.visit(
      label = Some("tuple"),
      tpe = TypeRepr.of[A],
      valueTerm = valueExpr.asTerm,
      functionWhenTuple = { (tpe, name, value, index) =>
        cache.put {
          transformToMatchTerm(
            tpe,
            value,
            functionOnCase = { (tpe, value) =>
              val messageTerm = StringUtils.concat(
                Literal(StringConstant("case _: ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                Literal(StringConstant(" => ")),
                value
              )
              MethodUtils.methodCall(bufferRef, "append", List(messageTerm))
            }
          )
        }
      },
      functionWhenNamedTuple = { (tpe, name, value, index) =>
        cache.put {
          transformToMatchTerm(
            tpe,
            value,
            functionOnCase = { (tpe, value) =>
              val messageTerm = StringUtils.concat(
                Literal(StringConstant("named case " + name.getOrElse("unknown") + ": ")),
                Literal(StringConstant(tpe.show(using Printer.TypeReprShortCode))),
                Literal(StringConstant(" => ")),
                value
              )
              MethodUtils.methodCall(bufferRef, "append", List(messageTerm))
            }
          )
        }
      },
      onStart = '{}.asTerm,
      onEnd = '{}.asTerm
    )

    cache.put {
      MethodUtils.methodCall(bufferRef, "mkString", List(Literal(StringConstant(", "))))
    }

    val result = cache.asTerm
    // report.warning(result.show(using Printer.TreeCode))
    result.asExprOf[String]
  }

}
