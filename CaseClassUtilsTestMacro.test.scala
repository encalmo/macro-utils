package org.encalmo.utils

import CaseClassUtils.*
import scala.quoted.*
import org.encalmo.utils.AnnotationUtils.AnnotationInfo

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
      { [A: Type] => (name, value, annotations) =>
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
      transformToListOfExpr[A, String](
        valueExpr,
        { [A: Type] => (name, value, annotations) =>
          Some('{
            ${ name } + ": " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } + " = " + ${
              value
            }.toString
          })
        }
      )
    )
  }

  inline def printCaseClassFields[T](inline value: T): Unit = ${ printCaseClassFieldsImpl('value) }

  def printCaseClassFieldsImpl[T: Type](value: Expr[T])(using Quotes): Expr[Unit] = {
    CaseClassUtils.visit[T](
      value,
      [A: Type] =>
        (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
          '{ println(${ nameExpr } + ": " + ${ valueExpr }.toString) }
    )
  }

  inline def upperCaseStringFields[T, R <: Product](inline value: T): R =
    ${ upperCaseStringFieldsImpl[T, R]('value) }

  def upperCaseStringFieldsImpl[T: Type, R <: Product: Type](value: Expr[T])(using Quotes): Expr[R] = {
    import quotes.reflect.*
    val args = CaseClassUtils.transformToList[T, quotes.reflect.Term](
      value,
      [A: Type] =>
        (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
          Some {
            Type.of[A] match {
              case '[String] =>
                '{ ${ valueExpr.asExprOf[String] }.toUpperCase }.asTerm
              case _ =>
                valueExpr.asTerm
            }
          }
    )

    CaseClassUtils.createInstanceUsingConstructor[R](args)
  }

  inline def upperCaseStringFields2[T, R <: Product](inline value: T): R =
    ${ upperCaseStringFields2Impl[T, R]('value) }

  def upperCaseStringFields2Impl[T: Type, R <: Product: Type](value: Expr[T])(using Quotes): Expr[R] = {
    val tuple = CaseClassUtils.transformToExprOfTuple[T](
      value,
      [A: Type] =>
        (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
          Some {
            Type.of[A] match
              case '[String] =>
                '{ ${ valueExpr.asExprOf[String] }.toUpperCase }
              case _ =>
                valueExpr
          }
    )
    CaseClassUtils.createInstanceFromTuple[R](tuple)
  }

  inline def extractFieldTuple[T](inline value: T): Tuple =
    ${ extractFieldTupleImpl[T]('value) }

  def extractFieldTupleImpl[T: Type](value: Expr[T])(using Quotes): Expr[Tuple] = {
    CaseClassUtils.transformToExprOfTuple[T](
      value,
      [A: Type] =>
        (nameExpr: Expr[String], valueExpr: Expr[A], annotations: Set[AnnotationInfo]) =>
          // For demonstration, we just put the value itself into the tuple
          Some(valueExpr)
    )
  }
}
