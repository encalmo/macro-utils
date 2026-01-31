package org.encalmo.utils

import scala.quoted.*

object JavaIterableUtils {

  def maybeVisitJavaIterable[T: Type](using
      Quotes
  )(
      functionExpr: [A: Type] => Quotes ?=> Expr[Unit]
  ): Option[Expr[Unit]] = {
    import quotes.reflect.*

    val javaListSymbol = Symbol.requiredClass("java.lang.Iterable")

    TypeRepr.of[T].dealias match {
      case AppliedType(base, List(innerType)) if base.typeSymbol == javaListSymbol =>
        innerType.asType match {
          case '[t] =>
            Some(functionExpr.apply[t])
        }

      case t if t <:< TypeRepr.typeConstructorOf(classOf[java.util.List[?]]) =>
        t.baseType(javaListSymbol) match {
          case AppliedType(_, List(innerType)) =>
            innerType.asType match {
              case '[t] => Some(functionExpr.apply[t])
            }
          case _ => Some(functionExpr.apply[Object])
        }
      case _ => None
    }
  }
}
