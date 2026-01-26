/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
