package org.encalmo.utils

import scala.quoted.*

object MethodUtils {

  /** Wrap a method call in a method definition and return the result of the method call. */
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

  /** Maybe select a value from expression using a selector and call the provided function if found, 
   * or call fallback function if not found. */
  def maybeSelectedValue[T: Type](
      selector: String,
      label: Expr[String],
      expr: Expr[T],
      functionExpr: [A: Type] => Quotes ?=> (Expr[String], Expr[A]) => Expr[Unit],
      fallbackExpr: => Expr[Unit]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T].dealias
    val classSymbol = tpe.typeSymbol

    // Try to find a field with the selector name.
    val fieldSymbol = classSymbol.fieldMember(selector)
    val targetSymbol =
      if (fieldSymbol.exists && !fieldSymbol.flags.is(Flags.Private | Flags.PrivateLocal))
      then Some(fieldSymbol)
      else {
        classSymbol
          .methodMember(selector)
          .find { sym =>
            val params = sym.paramSymss
            // Matches: def foo: T (Nil) OR def foo(): T (List(Nil))
            params == Nil || params == List(Nil)
          }
          .orElse {
            val getterName = s"get${selector.capitalize}"
            classSymbol.methodMember(getterName).find { sym =>
              val params = sym.paramSymss
              // Matches: def getFoo: T (Nil) OR def getFoo(): T (List(Nil))
              params == Nil || params == List(Nil)
            }
          }
      }

    targetSymbol match {
      case Some(sym) =>
        val actualType: TypeRepr =
          tpe.memberType(sym) match {
            case MethodType(_, _, res) => res
            case ByNameType(u)         => u
            case PolyType(_, _, res)   => res
            case other                 => other
          }

        val select = Select(expr.asTerm, sym)
        val term = if (sym.paramSymss == List(Nil)) 
          then Apply(select, Nil) 
          else select   

        actualType.asType match {
          case '[a] =>
            functionExpr.apply[a](Expr(selector), term.asExprOf[a])
        }

      case None => fallbackExpr
    }
  }

}
