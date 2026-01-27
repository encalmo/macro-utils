package org.encalmo.utils

import MethodUtils.*
import scala.quoted.*

object MethodUtilsTestMacro {

  inline def testMaybeSelectedValue[T](selector: String, expr: T): String = {
    ${ testMaybeSelectedValueImpl[T]('{ selector }, '{ expr }) }
  }

  def testMaybeSelectedValueImpl[T: Type](selectorExpr: Expr[String], exprExpr: Expr[T])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    '{
      var result: String = ""
      ${
        maybeSelectedValue(
          selectorExpr.valueOrAbort, 
          selectorExpr, 
          exprExpr, 
          functionExpr = { [A: Type] => Quotes ?=> (name, value) =>
            '{ result = ${ name } + ": " + ${ Expr(TypeRepr.of[A].show(using Printer.TypeReprShortCode)) } 
            + " = " + ${value}}},
          fallbackExpr = '{ result ="selector not found" }
        )
      }
      result
    }
  }
}
