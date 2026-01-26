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

object SelectableUtils {

  def maybeVisitSelectable[T: Type](using
      Quotes
  )(
      functionExpr: [Fields: Type] => Quotes ?=> Expr[Unit]
  ): Option[Expr[Unit]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[T].dealias

    def findFieldsType(t: TypeRepr): Option[TypeRepr] = t match {
      case Refinement(_, "Fields", bounds: TypeBounds) => Some(bounds.hi)
      case Refinement(parent, _, _)                    => findFieldsType(parent)
      case _                                           =>
        val fields = t.typeSymbol.typeMember("Fields")
        if (fields.exists)
        then {
          t.memberType(fields) match {
            case bounds: TypeBounds => Some(bounds.hi)
            case _                  => None
          }
        } else {
          None
        }
    }

    if (tpe <:< TypeRepr.of[Selectable]) {
      findFieldsType(tpe)
        .map { fieldsType =>
          fieldsType.asType match {
            case '[fields] => functionExpr.apply[fields]
          }
        }
    } else {
      None
    }
  }

  def visitFields[In: Type, Fields: Type](using
      Quotes
  )(
      valueExpr: Expr[In],
      functionExpr: [A: Type] => Quotes ?=> (Expr[String], Expr[A]) => Expr[Unit]
  ): Expr[Unit] = {
    import quotes.reflect.*
    TypeRepr.of[Fields].dealias match {
      case AppliedType(_, List(AppliedType(_, nameTypeList), AppliedType(_, valueTypeList))) =>

        val term = valueExpr.asTerm
        val selectDynamicSym = term.tpe.typeSymbol
          .methodMember("selectDynamic")
          .headOption
          .getOrElse(report.errorAndAbort(s"Method selectDynamic not found on ${term.tpe.show}"))

        Expr.block(
          nameTypeList
            .zip(valueTypeList)
            .zipWithIndex
            .map { case ((nameType, valueType), index) =>
              val name = TypeNameUtils.shortBaseName(nameType.show(using Printer.TypeReprShortCode))
              valueType.asType match {
                case '[value] =>
                  functionExpr.apply[value](
                    Expr(name), {
                      val arg = Literal(StringConstant(name))
                      val methodCall = Apply(Select(term, selectDynamicSym), List(arg))
                      '{ ${ methodCall.asExpr }.asInstanceOf[value] }.asExprOf[value]
                    }
                  )
              }
            },
          '{}
        )

      case _ =>
        '{}
    }
  }
}
