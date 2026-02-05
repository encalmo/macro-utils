package org.encalmo.utils

import scala.quoted.*
import org.encalmo.utils.TypeUtils.TypeReprIsPrimitiveOrStringOrBigDecimal

object TypeUtilsTestMacro {

  inline def testTypeReprIsPrimitiveOrStringOrBigDecimal[A]: Boolean = ${
    testTypeReprIsPrimitiveOrStringOrBigDecimalImpl[A]
  }
  def testTypeReprIsPrimitiveOrStringOrBigDecimalImpl[A: Type](using qctx: Quotes): Expr[Boolean] = {
    import qctx.reflect.*
    Expr(TypeReprIsPrimitiveOrStringOrBigDecimal.unapply(TypeRepr.of[A]))
  }
}
