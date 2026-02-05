package org.encalmo.utils

import scala.quoted.*

object OpaqueTypeUtils {

  /** Check if a type is an opaque type and return the underlying upper bound type if it is. */
  object TypeReprIsOpaqueType {
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Option[quotes.reflect.TypeRepr]] = {
      import quotes.reflect.*
      if (tpe.typeSymbol.flags.is(Flags.Opaque))
      then
        Some {
          val bases = tpe.baseClasses
          val underlyingClass = bases.find { s =>
            s != defn.AnyClass && s != defn.MatchableClass && s != defn.ObjectClass
          }
          underlyingClass match {
            case Some(superSym) =>
              val bound = tpe.baseType(superSym)
              Some(bound)

            case None =>
              None
          }
        }
      else None
    }
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
  def visit(using
      cache: StatementsCache
  )(
      label: String,
      tpe: cache.quotes.reflect.TypeRepr,
      valueTerm: cache.quotes.reflect.Term,
      functionWhenOpaqueType: (
          cache.quotes.reflect.TypeRepr,
          String,
          cache.quotes.reflect.Term
      ) => Unit,
      functionOtherwise: (cache.quotes.reflect.TypeRepr, String, cache.quotes.reflect.Term) => Unit
  ): Unit = {
    given cache.quotes.type = cache.quotes

    TypeUtils
      .underlyingTypeRepr(tpe)
      .match {
        case Left((tpe)) =>
          functionWhenOpaqueType(
            tpe,
            label,
            valueTerm
          )

        case _ =>
          functionOtherwise(
            tpe,
            label,
            valueTerm
          )
      }
  }

}
