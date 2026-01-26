package org.encalmo.utils

import scala.quoted.*

object DebugUtils {

  inline def printTree[T](inline x: T): Unit = ${ printTreeImpl('x) }
  def printTreeImpl[T: Type](x: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.*
    val output = x.asTerm.show(using Printer.TreeStructure)
    '{ println(${ Expr(output) }) }
}
