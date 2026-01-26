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
}
