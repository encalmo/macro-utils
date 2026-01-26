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

object UnionUtils {

  /** Check if a type is a union. */
  def isUnion[A: Type](using Quotes): Boolean = {
    import quotes.reflect.*
    TypeRepr.of[A].dealias match {
      case OrType(_, _) => true
      case _            => false
    }
  }

  /** Visit an union and create a pattern match of each union type.
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
      functionExpr: [A: Type] => Quotes ?=> (
          Expr[String],
          Expr[A]
      ) => Expr[Any]
  )(using quotes: Quotes): Expr[Unit] = {
    import quotes.reflect.*

    val matchCaseDefs =
      TypeUtils
        .inspectUnionType[In]
        .getOrElse(Nil)
        .map { tpe =>
          val typeTree = tpe.asType match {
            case '[t] => TypeTree.of[t]
          }
          val matchCasePattern = Typed(Wildcard(), typeTree)
          val matchCaseBody =
            tpe.asType match {
              case '[t] =>
                functionExpr.apply[t](
                  label,
                  '{ $valueExpr.asInstanceOf[t] }
                )
            }

          CaseDef(matchCasePattern, None, matchCaseBody.asTerm)
        }

    Match(valueExpr.asTerm, matchCaseDefs).asExprOf[Unit]
  }

}
