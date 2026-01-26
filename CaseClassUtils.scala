

package org.encalmo.utils

import scala.quoted.*

object CaseClassUtils {

  /** Check if a type is a case class. */
  def isCaseClass[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias.typeSymbol.isClassDef
    && TypeRepr.of[A].dealias.typeSymbol.flags.is(Flags.Case)
  }

  /** Transform a case class into a list of output values returned by the provided function. Filter out returned values
    * that are None.
    *
    * @param value
    *   The instance of the case class to transform.
    * @param function
    *   The function to apply to each field.
    * @return
    *   A list of output values.
    */

  def transform[In: Type, Out: Type](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A],
          List[quotes.reflect.Term]
      ) => Option[Expr[Out]]
  )(using quotes: Quotes): List[Expr[Out]] = {
    import quotes.reflect.*
    val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
      case Left(tpe)  => tpe
      case Right(tpe) => tpe
    }
    parentTpe.asType match {
      case '[p] =>
        parentTpe.typeSymbol.caseFields
          .map { caseField =>
            val tpe = parentTpe.memberType(caseField).dealias
            tpe.asType match {
              case '[t] =>
                functionExpr.apply[t](
                  Expr(caseField.name),
                  Select(valueExpr.asTerm, caseField).asExprOf[t],
                  AnnotationUtils.getFieldAnnotations[p](caseField.name)
                )
            }
          }
          .collect { case Some(expr) => expr }
    }
  }

  /** Visit a case class and apply a function to each field.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def visit[In: Type](
      valueExpr: Expr[In],
      functionExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A],
          List[quotes.reflect.Term]
      ) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*
    Expr.block(
      {
        val parentTpe = TypeUtils.underlyingTypeRepr[In] match {
          case Left(tpe)  => tpe
          case Right(tpe) => tpe
        }
        parentTpe.asType match {
          case '[p] =>
            parentTpe.typeSymbol.caseFields
              .map { caseField =>
                val tpe = parentTpe.memberType(caseField).dealias
                tpe.asType match {
                  case '[t] =>
                    functionExpr.apply[t](
                      Expr(caseField.name),
                      Select(valueExpr.asTerm, caseField).asExprOf[t],
                      AnnotationUtils.getFieldAnnotations[p](caseField.name)
                    )
                }
              }
        }
      },
      '{}
    )
  }

}
