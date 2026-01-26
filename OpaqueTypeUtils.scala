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

object OpaqueTypeUtils {

  /** Visit an opaque type.
    *
    * @param valueExpr
    * @param functionExpr
    * @param quotes
    * @return
    *   Unit
    */
  def visit[In: Type](
      label: Expr[String],
      valueExpr: Expr[In],
      functionWhenOpaqueTypeExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A]
      ) => Expr[Any],
      functionWhenOtherExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A]
      ) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    TypeUtils
      .underlyingTypeRepr[In]
      .match {
        case Left((tpe)) =>
          tpe.asType match {
            case '[t] =>
              functionWhenOpaqueTypeExpr.apply[t](
                label,
                valueExpr.asExprOf[t]
              )
          }

        case _ =>
          functionWhenOtherExpr.apply[In](
            label,
            valueExpr
          )
      }
      .asExprOf[Unit]
  }
}
