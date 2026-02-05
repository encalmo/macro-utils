package org.encalmo.utils

import scala.quoted.*

object OpaqueTypeUtils {

  /** Visit an opaque type and collect the results into a block of unit.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def transformToMatchExpression[In: Type](
      label: Expr[String],
      valueExpr: Expr[In],
      functionWhenOpaqueTypeExpr: [A: Type] => (Expr[String], Expr[A]) => Expr[Any],
      functionWhenOtherExpr: [A: Type] => (Expr[String], Expr[A]) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    TypeUtils
      .underlyingTypeRepr[In]
      .match {
        case Left((tpe)) =>
          tpe.asType match {
            case '[t] =>
              functionWhenOpaqueTypeExpr.apply[t](
                label,
                valueExpr.asExprOf[t]
              )
          }

        case _ =>
          functionWhenOtherExpr.apply[In](
            label,
            valueExpr
          )
      }
      .asExprOf[Unit]
  }

  /** Visit an opaque type and apply a dedicated function when the type is an opaque type or when the type is not an
    * opaque type.
    *
    * @param valueExpr
    * @param functionExpr
    * @param cache
    * @return
    *   Unit
    */
  def visit[In: Type](using
      cache: StatementsCache
  )(
      label: String,
      valueTerm: cache.quotes.reflect.Term,
      functionWhenOpaqueTypeExpr: [A: Type] => (
          cache.quotes.reflect.TypeRepr,
          String,
          cache.quotes.reflect.Term
      ) => Unit,
      functionWhenOtherExpr: [A: Type] => (String, cache.quotes.reflect.Term) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes

    TypeUtils
      .underlyingTypeRepr[In]
      .match {
        case Left((tpe)) =>
          tpe.asType match {
            case '[t] =>
              functionWhenOpaqueTypeExpr.apply[t](
                tpe,
                label,
                valueTerm
              )
          }

        case _ =>
          functionWhenOtherExpr.apply[In](
            label,
            valueTerm
          )
      }
  }

}
