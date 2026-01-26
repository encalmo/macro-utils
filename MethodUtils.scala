package org.encalmo.utils

import scala.quoted.*

object MethodUtils {

  def wrapInMethodCall[T: Type](
      methodName: String,
      methodBody: Expr[T]
  )(using quotes: Quotes): Expr[T] = {
    import quotes.reflect.*

    val methodSymbol: Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName,
      MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]),
      Flags.Synthetic,
      Symbol.noSymbol
    )

    val methodDef = DefDef(
      methodSymbol,
      { _ => Some(methodBody.asTerm.changeOwner(methodSymbol)) }
    )

    Block(
      List(methodDef),
      Apply(Ref(methodSymbol), Nil)
    ).asExprOf[T]
  }

}
