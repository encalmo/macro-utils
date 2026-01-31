package org.encalmo.utils

import scala.quoted.*

object OpaqueTypeUtils {

  /** Visit an opaque type.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def visit[In: Type](
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
}
