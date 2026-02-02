package org.encalmo.utils

import scala.quoted.*

object JavaMapUtils {

  def maybeVisitJavaMap[T: Type](using
      Quotes
  )(
      functionExpr: [K: Type, V: Type] => Quotes ?=> Expr[Unit]
  ): Option[Expr[Unit]] = {
    import quotes.reflect.*

    val javaMapSymbol = Symbol.requiredClass("java.util.Map")

    val tpe = TypeRepr.of[T].dealias
    if (tpe <:< TypeRepr.typeConstructorOf(classOf[java.util.Map[?, ?]])) {
      Some {
        tpe.baseType(javaMapSymbol) match {
          case AppliedType(_, List(keyType, valueType)) =>
            keyType.asType match {
              case '[k] =>
                valueType.asType match {
                  case '[v] =>
                    functionExpr.apply[k, v]
                }
            }

          case _ =>
            functionExpr.apply[Object, Object]
        }
      }
    } else {
      None
    }
  }

  def maybeVisitJavaMap[T: Type](using
      cache: StatementsCache
  )(
      functionExpr: [K: Type, V: Type] => StatementsCache ?=> Unit
  ): Option[Unit] = {
    given cache.quotes.type = cache.quotes
    import cache.quotes.reflect.*

    val javaMapSymbol = Symbol.requiredClass("java.util.Map")

    val tpe = TypeRepr.of[T].dealias
    if (tpe <:< TypeRepr.typeConstructorOf(classOf[java.util.Map[?, ?]])) {
      Some {
        tpe.baseType(javaMapSymbol) match {
          case AppliedType(_, List(keyType, valueType)) =>
            keyType.asType match {
              case '[k] =>
                valueType.asType match {
                  case '[v] =>
                    functionExpr.apply[k, v]
                }
            }

          case _ =>
            functionExpr.apply[Object, Object]
        }
      }
    } else {
      None
    }
  }
}
