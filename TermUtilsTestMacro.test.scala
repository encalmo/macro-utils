package org.encalmo.utils

import scala.quoted.*
import StatementsCache.*

object TermUtilsTestMacro {

  inline def testIsLiteralConstant[A](inline expr: A): Boolean = {
    ${ testIsLiteralConstantImpl[A]('{ expr }) }
  }

  def testIsLiteralConstantImpl[A: Type](expr: Expr[A])(using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    val cache: StatementsCache = new StatementsCache
    Expr(TermUtils.isLiteralConstant(using cache)(expr.asTerm.toTerm(using cache)))
  }

  inline def testIsNullConstant[A](inline expr: A): Boolean = {
    ${ testIsNullConstantImpl[A]('{ expr }) }
  }

  def testIsNullConstantImpl[A: Type](expr: Expr[A])(using Quotes): Expr[Boolean] = {
    import quotes.reflect.*
    val cache: StatementsCache = new StatementsCache
    val term = expr.asTerm.toTerm(using cache)
    Expr(TermUtils.isNullConstant(using cache)(term))
  }

}
